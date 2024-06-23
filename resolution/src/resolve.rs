use ahash::{HashMap, HashMapExt};
use common::PurrSource;
use error::CompilerError;
use parser::{ast::{self, NodeId}, parser::ParseNotes};
use crate::project_tree::{ProjectTree, ResolutionPath};
use super::ResolvedTy;

#[derive(Debug, Clone, PartialEq)]
pub struct ResolutionNotes<'a> {
    current_path: ResolutionPath,
    current_file: PurrSource,
    project_tree: &'a ProjectTree,
    ast: &'a Vec<ast::Item>
}

#[derive(Default, Debug, Clone)]
pub struct ResolvedData {
    pub types: HashMap<NodeId, ResolvedTy>,
    pub variables: HashMap<NodeId, NodeId>
}

impl<'a> ResolutionNotes<'a> {
    pub fn from_ast(ast: &'a (Vec<ast::Item>, ParseNotes), project_tree: &'a ProjectTree) -> Self {
        Self {
            current_path: ResolutionPath::default(),
            current_file: ast.1.file.clone(),
            project_tree,
            ast: &ast.0
        }
    }
}

pub struct Stack {
    ribs: Vec<Rib>
}

impl Stack {
    pub fn new() -> Self {
        Self {
            ribs: Vec::new()
        }
    }

    pub fn find_variable_origin(&self, name: &str) -> Option<NodeId> {
        for rib in self.ribs.iter().rev() {
            if let Some(node_id) = rib.variables.get(name) {
                return Some(*node_id)
            }
        }
        None
    }

    pub fn push(&mut self) {
        self.ribs.push(Rib::new())
    }
    pub fn pop(&mut self) {
        self.ribs.pop();
    }

    pub fn top(&mut self) -> &mut Rib {
        if self.ribs.len() == 0 {
            self.ribs.push(Rib::new());
        }

        self.ribs.last_mut().unwrap()
    }
}

pub struct Rib {
    variables: HashMap<String, NodeId>
}

impl Rib {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new()
        }
    }

    pub fn define_variable(&mut self, name: &str, id: NodeId) {
        self.variables.insert(name.to_string(), id);
    }
}

pub fn resolve(
    ast: &(Vec<ast::Item>, ParseNotes),
    project_tree: &ProjectTree
) -> Result<ResolvedData, CompilerError> {
    let mut data = ResolvedData::default();
    let notes = ResolutionNotes::from_ast(ast, project_tree);

    resolve_tl_types(&mut data, &notes)?;
    resolve_item_statements(&mut data, &notes)?;

    Ok(data)
}

/// This resolves top level (easy) types.
/// There is NO type inference at this level.
/// It allows It to occur during the later steps.
pub fn resolve_tl_types(
    resolved: &mut ResolvedData,
    notes: &ResolutionNotes
) -> Result<(), CompilerError> {
    for item in notes.ast.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                resolve_tl_types(
                    resolved,
                    &ResolutionNotes {
                        current_path: notes.current_path.clone()
                            .join(&[module.name.as_str()].as_slice().into()),
                        current_file: module.source.clone(),
                        project_tree: notes.project_tree,
                        ast: &module.body,
                    }
                )?;
            },
            ast::ItemKind::BlockDefinition(block) => {
                resolved.types.insert(
                    item.id,
                    signature_to_resolved_ty(&block.signature, notes)?
                );
            },
            ast::ItemKind::FunctionDefinition(function) => {
                resolved.types.insert(
                    item.id,
                    signature_to_resolved_ty(&function.signature, notes)?
                );
            },
            ast::ItemKind::StructDefinition(struct_) => {
                resolved.types.insert(
                    item.id,
                    ResolvedTy::from_ast_ty(
                        &ast::Ty {
                            kind: ast::TyKind::AnonStruct(struct_.fields.clone()),
                            pos: 0..0
                        },
                        &notes.current_path,
                        &notes.project_tree,
                        &notes.current_file
                    )?
                );
            },
            _ => {}
        }
    }

    Ok(())
}

pub fn resolve_item_statements(
    resolved: &mut ResolvedData,
    notes: &ResolutionNotes
) -> Result<(), CompilerError> {
    for item in notes.ast.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                resolve_item_statements(
                    resolved,
                    &ResolutionNotes {
                        current_path: notes.current_path.clone()
                            .join(&[module.name.as_str()].as_slice().into()),
                        current_file: module.source.clone(),
                        project_tree: notes.project_tree,
                        ast: &module.body,
                    }
                )?;
            },
            ast::ItemKind::FunctionDefinition(definition) => {
                resolve_statements(
                    &definition.body,
                    resolved,
                    &mut Stack::new(),
                    notes
                )?;
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn resolve_statements(
    ast: &Vec<ast::Statement>,
    resolved: &mut ResolvedData,
    stack: &mut Stack,
    notes: &ResolutionNotes
) -> Result<(), CompilerError> {
    for stmt in ast.iter() {
        match &stmt.kind {
            ast::StatementKind::LetDefinition(definition) => {
                if definition.ty.kind != ast::TyKind::Infer {
                    resolved.types.insert(
                        stmt.id,
                        resolve_ty(&definition.ty, notes)?
                    );
                }

                if let Some(rhs) = &definition.value {
                    let return_type = resolve_expr(&rhs, resolved, stack, notes)?;
                    if let Some(lhs_ty) = resolved.types.get(&stmt.id) {
                        if return_type != *lhs_ty {
                            return Err(CompilerError::MismatchedTypes {
                                pos: stmt.pos.clone(),
                                lhs: definition.ty.pos.clone(),
                                lhs_ty: lhs_ty.pretty_name(),
                                rhs: rhs.pos.clone(),
                                rhs_ty: return_type.pretty_name(),
                                file: notes.current_file.clone()
                            });
                        }
                    } else {
                        resolved.types.insert(stmt.id, return_type);
                    }
                }

                stack.top().define_variable(&definition.symbol, stmt.id);
            },

            ast::StatementKind::Expr(expr) |
            ast::StatementKind::ExprNoSemi(expr) |
            ast::StatementKind::Return(Some(expr)) => {
                let expr_ty = 
                    resolve_expr(&expr, resolved, stack, notes)?;
                resolved.types.insert(
                    stmt.id, expr_ty
                );
            },
            _ => {}
        }
    }
    Ok(())
}

pub fn resolve_expr(
    expr: &ast::Expression,
    resolved: &mut ResolvedData,
    stack: &mut Stack,
    notes: &ResolutionNotes
) -> Result<ResolvedTy, CompilerError> {
    let resolved = match &expr.kind {
        ast::ExpressionKind::Number(_) => ResolvedTy::Number,
        ast::ExpressionKind::String(_) => ResolvedTy::Text,
        ast::ExpressionKind::Bool(_) => ResolvedTy::Bool,

        ast::ExpressionKind::Paren(subexpr) => 
            resolve_expr(subexpr, resolved, stack, notes)?,

        ast::ExpressionKind::Path(path) => {
            if path.segments.len() == 1 {
                if let Some(variable) = stack.find_variable_origin(
                    &path.segments.first().unwrap().ident
                ) {
                    resolved.variables.insert(expr.id, variable);
                    if let Some(resolved_ty) = resolved.types.get(&variable) {
                        return Ok(resolved_ty.clone());
                    }
                }
            }

            unimplemented!()
        },
        _ => { ResolvedTy::Void }
    };
    Ok(resolved)
}

fn resolve_ty(
    ty: &ast::Ty,
    notes: &ResolutionNotes
) -> Result<ResolvedTy, CompilerError> {
    ResolvedTy::from_ast_ty(
        ty,
        &notes.current_path,
        &notes.project_tree,
        &notes.current_file
    )
}

fn signature_to_resolved_ty(
    signature: &ast::Signature,
    notes: &ResolutionNotes
) -> Result<ResolvedTy, CompilerError> {
    let mut args = Vec::new();

    for arg in signature.arguments.iter() {
        args.push(ResolvedTy::from_ast_ty(&arg.ty, &notes.current_path, &notes.project_tree, &notes.current_file)?);
    }

    Ok(ResolvedTy::Function(
        args,
        Box::new(
            ResolvedTy::from_ast_ty(&signature.return_type, &notes.current_path, &notes.project_tree, &notes.current_file)?
        )
    ))
}
