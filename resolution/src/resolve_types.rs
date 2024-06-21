use ahash::HashMap;
use common::PurrSource;
use error::CompilerError;
use parser::{ast::{self, NodeId}, parser::ParseNotes};
use crate::project_tree::{ProjectTree, ResolutionPath};
use super::ResolvedTy;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeResolutionNotes<'a> {
    current_path: ResolutionPath,
    current_file: PurrSource,
    project_tree: &'a ProjectTree,
    ast: &'a Vec<ast::Item>
}

#[derive(Default, Debug, Clone)]
pub struct ResolvedTypes {
    pub types: HashMap<NodeId, ResolvedTy>
}

impl<'a> TypeResolutionNotes<'a> {
    pub fn from_ast(ast: &'a (Vec<ast::Item>, ParseNotes), project_tree: &'a ProjectTree) -> Self {
        Self {
            current_path: ResolutionPath::default(),
            current_file: ast.1.file.clone(),
            project_tree,
            ast: &ast.0
        }
    }
}

pub fn resolve_types(
    ast: &(Vec<ast::Item>, ParseNotes),
    project_tree: &ProjectTree
) -> Result<ResolvedTypes, CompilerError> {
    let mut types = ResolvedTypes::default();
    let notes = TypeResolutionNotes::from_ast(ast, project_tree);

    resolve_tl_types(&mut types, &notes)?;

    Ok(types)
}

/// This resolves top level (easy) types.
/// There is NO type inference at this level.
/// It allows It to occur during the later steps.
pub fn resolve_tl_types(
    resolved: &mut ResolvedTypes,
    notes: &TypeResolutionNotes
) -> Result<(), CompilerError> {
    for item in notes.ast.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                resolve_tl_types(
                    resolved,
                    &TypeResolutionNotes {
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

fn signature_to_resolved_ty(
    signature: &ast::Signature,
    notes: &TypeResolutionNotes
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
