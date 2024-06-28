use ahash::{HashMap, HashMapExt};
use common::{FileRange, PurrSource};
use error::{create_error, info::CodeArea, CompilerError};
use parser::{ast::{self, NodeId}, parser::ParseNotes};
use crate::project_tree::{ProjectTree, ResolutionPath};
use super::ResolvedTy;

#[derive(Debug, Clone, PartialEq)]
pub struct ResolutionNotes<'a> {
    current_path: ResolutionPath,
    current_file: PurrSource,
    project_tree: &'a ProjectTree,
    expected_ret_ty: Option<(ResolvedTy, FileRange)>,
    expected_block_ty: Option<(ResolvedTy, FileRange)>,
    default_ret_ty: ResolvedTy,
    ast: &'a Vec<ast::Item>,
    struct_field_positions: HashMap<NodeId, HashMap<String, FileRange>>,
    items_positions: HashMap<NodeId, FileRange>
}

#[derive(Default, Debug, Clone)]
pub struct ResolvedData {
    pub types: HashMap<NodeId, ResolvedTy>,
    pub names: HashMap<NodeId, String>,
    pub variables: HashMap<NodeId, NodeId>,
    pub blocks: HashMap<NodeId, ResolvedBlock>,
    pub signatures: HashMap<NodeId, Vec<NodeId>>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedBlock {
    pub opcode: String,
    pub inputs: Vec<String>,
}

impl<'a> ResolutionNotes<'a> {
    pub fn from_ast(ast: &'a (Vec<ast::Item>, ParseNotes), project_tree: &'a ProjectTree) -> Self {
        Self {
            current_path: ResolutionPath::default(),
            current_file: ast.1.file.clone(),
            project_tree,
            expected_ret_ty: None,
            expected_block_ty: None,
            default_ret_ty: ResolvedTy::Void,
            ast: &ast.0,
            struct_field_positions: HashMap::new(),
            items_positions: HashMap::new()
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

    pub fn from_signature(signature: &ast::Signature) -> Self {
        let mut stack = Self::new();
        for argument in signature.arguments.iter() {
            stack.top().define_variable(&argument.name, argument.id);
        }
        stack
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
    let mut notes = ResolutionNotes::from_ast(ast, project_tree);

    resolve_tl_types(&mut data, &mut notes)?;
    resolve_item_statements(&mut data, &mut notes)?;

    Ok(data)
}

/// This resolves top level (easy) types.
/// There is NO type inference at this level.
/// It allows It to occur during the later steps.
pub fn resolve_tl_types(
    resolved: &mut ResolvedData,
    notes: &mut ResolutionNotes
) -> Result<(), CompilerError> {
    for item in notes.ast.iter() {
        notes.items_positions.insert(item.id, item.pos.clone());
        match &item.kind {
            ast::ItemKind::Module(module) => {
                let path_temp = notes.current_path.clone();
                notes.current_path = notes.current_path.clone()
                    .join(&[module.name.as_str()].as_slice().into());
                let ast_temp = notes.ast;
                notes.ast = &module.body;
                resolve_tl_types(
                    resolved,
                    notes
                )?;
                notes.current_path = path_temp;
                notes.ast = ast_temp;
            },
            ast::ItemKind::BlockDefinition(block) => {
                let ty = signature_to_resolved_ty(&block.signature, resolved, notes)?;
                resolved.types.insert(
                    item.id, ty
                );
                resolved.signatures.insert(
                    item.id,
                    block.signature.arguments.iter().map(|arg| arg.id).collect()
                );
            },
            ast::ItemKind::FunctionDefinition(function) => {
                let ty = signature_to_resolved_ty(&function.signature, resolved, notes)?;
                resolved.types.insert(
                    item.id, ty
                );

                resolved.names.insert(item.id, function.name.clone());

                resolved.signatures.insert(
                    item.id,
                    function.signature.arguments.iter().map(|arg| arg.id).collect()
                );
            },
            ast::ItemKind::StructDefinition(struct_) => {
                let mut map = HashMap::new();
                for field in struct_.fields.iter() {
                    map.insert(field.name.clone(), field.pos.clone());
                }
                notes.struct_field_positions.insert(item.id, map);
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
    notes: &mut ResolutionNotes
) -> Result<(), CompilerError> {
    for item in notes.ast.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                let path_temp = notes.current_path.clone();
                notes.current_path = notes.current_path.clone()
                    .join(&[module.name.as_str()].as_slice().into());
                let ast_temp = notes.ast;
                notes.ast = &module.body;
                resolve_item_statements(
                    resolved,
                    notes
                )?;
                notes.current_path = path_temp;
                notes.ast = ast_temp;
            },

            ast::ItemKind::BlockDefinition(block) => {
                let mut stack = Stack::from_signature(&block.signature);
                let mut inputs = vec![
                    "".to_string();
                    block.signature.arguments.len()
                ];

                if let Some(inputs_field) = block.body.iter().find(|f| f.name == "inputs") {
                    let ast::ExpressionKind::AnonStruct(fields) = &inputs_field.value.kind
                        else { panic!("Value for inputs is not AnonStruct") };
                    for field in fields.iter() {
                        let ast::ExpressionKind::Path(path) = &field.value.kind else {
                            todo!("Create error for when block.inputs is not a path. Got {:?}", field.value);
                        };
                        // This guarantees this expression exists.
                        let _ = resolve_expr(&field.value, resolved, &mut stack, notes)?;
                        // TODO: Ensure this path is of length 1.
                        let origin = stack.find_variable_origin(&path.segments.last().unwrap().ident);
                        let origin_index = block.signature.arguments.iter()
                            .position(|arg| Some(arg.id) == origin);
                        if let Some(origin_index) = origin_index {
                            if inputs[origin_index] != "" {
                                panic!("Temporary Error: Block arguments may be used only once") 
                            }
                            inputs[origin_index] = field.name.clone();
                        } else { unreachable!("Block definition inputs value cannot be path/undefined.") }
                    }
                }

                resolved.blocks.insert(
                    item.id,
                    ResolvedBlock {
                        opcode: block.opcode.clone(),
                        inputs
                    }
                );
            }

            ast::ItemKind::FunctionDefinition(definition) => {
                let ResolvedTy::Function(_, return_type) = 
                    signature_to_resolved_ty(&definition.signature, resolved, notes)?
                    else { unreachable!() };
                notes.expected_ret_ty = Some((
                    *return_type.clone(),
                    definition.signature.return_type.pos.clone()
                ));
                notes.expected_block_ty = Some((
                    *return_type.clone(),
                    definition.signature.return_type.pos.clone()
                ));
                let mut stack = Stack::from_signature(&definition.signature);
                let block_return_type = resolve_statements(
                    &definition.body,
                    resolved,
                    &mut stack,
                    notes
                )?;
                if !block_return_type.matches(&*return_type, resolved, notes) {
                    return Err(CompilerError::MismatchedTypes {
                        pos: item.pos.clone(),
                        lhs: definition.signature.return_type.pos.clone(),
                        lhs_ty: return_type.pretty_name(&notes.project_tree),
                        rhs: definition.body.last().map(|rhs| rhs.pos.clone())
                            .unwrap_or(item.pos.clone()).clone(),
                        rhs_ty: block_return_type.pretty_name(&notes.project_tree),
                        file: notes.current_file.clone()
                    });
                }
                notes.expected_ret_ty = None;
                notes.expected_block_ty = None;
            },

            ast::ItemKind::Trigger(trigger) => {
                notes.expected_ret_ty = Some((
                    ResolvedTy::Void,
                    item.pos.clone()
                ));
                notes.expected_block_ty = Some((
                    ResolvedTy::Void,
                    item.pos.clone()
                ));
                let block_return_type = resolve_statements(
                    &trigger.body,
                    resolved,
                    &mut Stack::new(),
                    notes
                )?;
                if !block_return_type.matches(&ResolvedTy::Void, resolved, notes) {
                    // TODO: Custom error?
                    return Err(CompilerError::MismatchedTypes {
                        pos: item.pos.clone(),
                        lhs: item.pos.clone(),
                        lhs_ty: ResolvedTy::Void.pretty_name(&notes.project_tree),
                        rhs: trigger.body.last().map(|rhs| rhs.pos.clone())
                            .unwrap_or(item.pos.clone()).clone(),
                        rhs_ty: block_return_type.pretty_name(&notes.project_tree),
                        file: notes.current_file.clone()
                    });
                }
                notes.expected_ret_ty = None;
                notes.expected_block_ty = None;
            },
            _ => {}
        }
    }
    Ok(())
}

pub fn resolve_statements(
    ast: &Vec<ast::Statement>,
    resolved: &mut ResolvedData,
    stack: &mut Stack,
    notes: &mut ResolutionNotes
) -> Result<ResolvedTy, CompilerError> {
    let mut block_return_type = notes.default_ret_ty.clone();
    for (i, stmt) in ast.iter().enumerate() {
        match &stmt.kind {
            ast::StatementKind::LetDefinition(definition) => {
                if definition.ty.kind != ast::TyKind::Infer {
                    resolved.types.insert(
                        stmt.id,
                        resolve_ty(&definition.ty, notes)?
                    );
                }

                if let Some(rhs) = &definition.value {
                    let return_type = resolve_expr(&rhs, resolved, stack, notes, )?;
                    if let Some(lhs_ty) = resolved.types.get(&stmt.id) {
                        if !return_type.matches(&lhs_ty, resolved, notes) {
                            return Err(CompilerError::MismatchedTypes {
                                pos: stmt.pos.clone(),
                                lhs: definition.ty.pos.clone(),
                                lhs_ty: lhs_ty.pretty_name(&notes.project_tree),
                                rhs: rhs.pos.clone(),
                                rhs_ty: return_type.pretty_name(&notes.project_tree),
                                file: notes.current_file.clone()
                            });
                        }
                    } else {
                        resolved.types.insert(stmt.id, return_type);
                    }
                }

                stack.top().define_variable(&definition.symbol, stmt.id);
            },

            ast::StatementKind::Expr(expr) => {
                let expr_ty = 
                    resolve_expr(&expr, resolved, stack, notes)?;
                resolved.types.insert(
                    stmt.id, expr_ty
                );
            },
            ast::StatementKind::ExprNoSemi(expr) => {
                if i != ast.len()-1 {
                    return Err(CompilerError::Custom(
                        create_error(
                            error::info::ErrorInfo::from_area(CodeArea {
                                pos: expr.pos.clone(), file: notes.current_file.clone()
                            }),
                            "Compilation error",
                            &[(
                                CodeArea { pos: expr.pos.clone(), file: notes.current_file.clone() },
                                "Expected semicolon after this expression.",
                            )],
                            Some("Returning (no-semi) expression are only allowed at the end of block.")
                        )
                    ));
                }
                let expr_ty = 
                    resolve_expr(&expr, resolved, stack, notes)?;
                if let Some((block_ty, block_pos)) = &notes.expected_block_ty {
                    if !expr_ty.matches(block_ty, resolved, notes) {
                        return Err(CompilerError::MismatchedTypes {
                            pos: stmt.pos.clone(),
                            lhs: block_pos.clone(),
                            lhs_ty: block_ty.pretty_name(&notes.project_tree),
                            rhs: expr.pos.clone(),
                            rhs_ty: expr_ty.pretty_name(&notes.project_tree),
                            file: notes.current_file.clone()
                        });
                    }
                }
                block_return_type = expr_ty.clone();
                resolved.types.insert(
                    stmt.id, expr_ty
                );
            }
            ast::StatementKind::Return(Some(expr)) => {
                let expr_ty = 
                    resolve_expr(&expr, resolved, stack, notes)?;
                if let Some((ret_ty, ret_pos)) = &notes.expected_ret_ty {
                    if !expr_ty.matches(&*ret_ty, resolved, notes) {
                        return Err(CompilerError::MismatchedTypes {
                            pos: stmt.pos.clone(),
                            lhs: ret_pos.clone(),
                            lhs_ty: ret_ty.pretty_name(&notes.project_tree),
                            rhs: expr.pos.clone(),
                            rhs_ty: expr_ty.pretty_name(&notes.project_tree),
                            file: notes.current_file.clone()
                        });
                    }
                }
                block_return_type = expr_ty.clone();
                resolved.types.insert(
                    stmt.id, expr_ty
                );
            },
            _ => {}
        }
    }
    Ok(block_return_type)
}

pub fn resolve_expr(
    expr: &ast::Expression,
    resolved: &mut ResolvedData,
    stack: &mut Stack,
    notes: &mut ResolutionNotes,
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

            let resolved_ty = resolve_ty(&ast::Ty {
                kind: ast::TyKind::Path(path.clone()),
                pos: 0..0
            }, notes)?;

            resolved.types.insert(expr.id, resolved_ty.clone());

            resolved_ty
        },

        ast::ExpressionKind::Field(obj, field) => {
            let obj_ty = resolve_expr(
                &obj,
                resolved,
                stack,
                notes
            )?;

            fn expected_struct<T>(
                expr: &ast::Expression,
                obj: &Box<ast::Expression>,
                ty: &ResolvedTy,
                notes: &ResolutionNotes
            ) -> Result<T, CompilerError> {
                Err(CompilerError::Custom(
                    create_error(
                        error::info::ErrorInfo::from_area(CodeArea {
                            pos: expr.pos.clone(), file: notes.current_file.clone()
                        }),
                        "Compilation error",
                        &[(
                            CodeArea { pos: obj.pos.clone(), file: notes.current_file.clone() },
                            &format!("Expected this to be struct, but got {}.",
                                ty.pretty_name(&notes.project_tree))
                        )],
                        None
                    )
                ))
            }

            let (struct_type, def_span) = {
                if let ResolvedTy::Path(def_path) = obj_ty {
                    let ResolvedTy::Struct(struct_type) = resolved.types.get(&def_path).unwrap().clone() else {
                        return expected_struct(expr, obj, &obj_ty, notes);
                    };
                    (
                        struct_type,
                        notes.items_positions.get(&def_path).unwrap().clone()
                    )
                } else {
                    let ResolvedTy::Struct(struct_type) = obj_ty else {
                        return expected_struct(expr, obj, &obj_ty, notes);
                    };
                    (struct_type, obj.pos.clone())
                }
            };

            if let Some(field_ty) = struct_type.get(field) {
                field_ty.clone()
            } else {
                return Err(CompilerError::UnknownField {
                    field: obj.pos.end..expr.pos.end,
                    definition: def_span,
                    file: notes.current_file.clone()
                });
            }
        },

        ast::ExpressionKind::Call { callee, generics: _generics, arguments } => {
            let callee_ty = resolve_expr(&callee, resolved, stack, notes)?;

            let ResolvedTy::Function(callee_args, callee_ret_ty) = 
                callee_ty.resolve_to_top(resolved) else {
                return Err(CompilerError::Custom(
                    create_error(
                        error::info::ErrorInfo::from_area(CodeArea {
                            pos: expr.pos.clone(), file: notes.current_file.clone()
                        }),
                        "Compilation error",
                        &[(
                            CodeArea { pos: callee.pos.clone(), file: notes.current_file.clone() },
                            "This expression is not callable.",
                        )],
                        Some("Only functions and blocks are callable.")
                    )
                ));
            };
            
            let mut argument_types = Vec::new();
            for arg in arguments.iter() {
                argument_types.push(resolve_expr(&arg, resolved, stack, notes)?);
            }

            if argument_types.len() != callee_args.len() {
                return Err(CompilerError::Custom(
                    create_error(
                        error::info::ErrorInfo::from_area(CodeArea {
                            pos: expr.pos.clone(), file: notes.current_file.clone()
                        }),
                        "Compilation error",
                        &[(
                            CodeArea { pos: expr.pos.clone(), file: notes.current_file.clone() },
                            &format!(
                                "This function expects {} arguments but got {}.",
                                callee_args.len(),
                                argument_types.len()
                            ),
                        )],
                        None
                    )
                ));
            }

            for (i, (callee_arg, call_arg)) in callee_args.iter().zip(argument_types.iter()).enumerate() {
                if !call_arg.matches(callee_arg, resolved, notes) {
                    return Err(CompilerError::Custom(
                        create_error(
                            error::info::ErrorInfo::from_area(CodeArea {
                                pos: expr.pos.clone(), file: notes.current_file.clone()
                            }),
                            "Compilation error",
                            &[(
                                CodeArea { pos: arguments[i].pos.clone(), file: notes.current_file.clone() },
                                "This argument does not match Its definition.",
                            )],
                            None
                        )
                    ));
                }
            }

            *callee_ret_ty.clone()
        },

        ast::ExpressionKind::StructLiteral(path, fields) => {
            let struct_ty = resolve_ty(&ast::Ty {
                kind: ast::TyKind::Path(path.clone()),
                pos: 0..0
            }, notes)?;

            let ResolvedTy::Path(struct_node) = struct_ty else { unreachable!() };
            let ResolvedTy::Struct(struct_type) = resolved.types.get(&struct_node).unwrap().clone() else {
                return Err(CompilerError::MismatchedTypes {
                    pos: expr.pos.clone(),
                    lhs: notes.items_positions.get(&struct_node).unwrap().clone(),
                    lhs_ty: resolved.types.get(&struct_node).unwrap().pretty_name(&notes.project_tree),
                    rhs: path.pos.clone(),
                    rhs_ty: "Struct Literal".to_string(),
                    file: notes.current_file.clone()
                });
            };

            for field in fields.iter() {
                let field_ty = resolve_expr(
                    &field.value,
                    resolved,
                    stack,
                    notes
                )?;

                let Some(field_expected_ty) = struct_type.get(&field.name) else {
                    return Err(CompilerError::UnknownField {
                        field: field.pos.clone(),
                        definition: notes.items_positions.get(&struct_node).unwrap().clone(),
                        file: notes.current_file.clone()
                    });
                };

                if !field_ty.matches(&*field_expected_ty, resolved, notes) {
                    return Err(CompilerError::MismatchedTypes {
                        pos: expr.pos.clone(),
                        lhs: notes.struct_field_positions.get(&struct_node).unwrap()
                            .get(&field.name).unwrap().clone(),
                        lhs_ty: field_expected_ty.pretty_name(&notes.project_tree),
                        rhs: field.pos.clone(),
                        rhs_ty: field_ty.pretty_name(&notes.project_tree),
                        file: notes.current_file.clone()
                    });
                }
            }
            struct_ty
        },

        ast::ExpressionKind::AnonStruct(fields) => {
            let mut fields_map = HashMap::new();
            for field in fields.iter() {
                fields_map.insert(
                    field.name.clone(),
                    resolve_expr(
                        &field.value, 
                        resolved, 
                        stack, 
                        notes
                    )?
                );
            }
            ResolvedTy::Struct(fields_map)
        },

        ast::ExpressionKind::Unary(op, subexpr) => {
            let expr_ty = resolve_expr(&subexpr, resolved, stack, notes)?;
            let ty = match op {
                ast::UnaryOp::Neg => {
                    if expr_ty != ResolvedTy::Number {
                        return Err(CompilerError::Custom(
                            create_error(
                                error::info::ErrorInfo::from_area(CodeArea {
                                    pos: expr.pos.clone(), file: notes.current_file.clone()
                                }),
                                "Compilation error",
                                &[(
                                    CodeArea { pos: subexpr.pos.clone(), file: notes.current_file.clone() },
                                    "'-' unary operator works only on numbers.",
                                )],
                                None
                            )
                        ));
                    }
                    ResolvedTy::Number
                }
                ast::UnaryOp::Not => {
                    if expr_ty != ResolvedTy::Bool {
                        return Err(CompilerError::Custom(
                            create_error(
                                error::info::ErrorInfo::from_area(CodeArea {
                                    pos: expr.pos.clone(), file: notes.current_file.clone()
                                }),
                                "Compilation error",
                                &[(
                                    CodeArea { pos: subexpr.pos.clone(), file: notes.current_file.clone() },
                                    "'!' unary operator works only on boolean values.",
                                )],
                                None
                            )
                        ));
                    }
                    ResolvedTy::Bool
                }
            };

            resolved.types.insert(expr.id, ty.clone());
            ty
        }

        ast::ExpressionKind::Binary(a, op, b) => {
            let a_ty = resolve_expr(&a, resolved, stack, notes)?;
            let b_ty = resolve_expr(&b, resolved, stack, notes)?;

            fn expect_two_numbers(
                expr: &ast::Expression,
                a: &ast::Expression,
                a_ty: &ResolvedTy,
                b: &ast::Expression,
                b_ty: &ResolvedTy,
                resolved: &ResolvedData,
                notes: &ResolutionNotes
            ) -> Result<(), CompilerError> {
                if a_ty.resolve_to_top(resolved) != ResolvedTy::Number {
                    return Err(CompilerError::MismatchedTypes {
                        pos: expr.pos.clone(),
                        lhs: a.pos.clone(),
                        lhs_ty: a_ty.pretty_name(&notes.project_tree),
                        rhs: expr.pos.clone(),
                        rhs_ty: "This expression requires both values to be a number.".to_string(),
                        file: notes.current_file.clone()
                    });
                }
                if b_ty.resolve_to_top(resolved) != ResolvedTy::Number {
                    return Err(CompilerError::MismatchedTypes {
                        pos: expr.pos.clone(),
                        lhs: expr.pos.clone(),
                        lhs_ty: "This expression requires both values to be a number.".to_string(),
                        rhs: b.pos.clone(),
                        rhs_ty: b_ty.pretty_name(&notes.project_tree),
                        file: notes.current_file.clone()
                    });
                }
                Ok(())
            }
            
            let ty = match op {
                ast::BinaryOp::Eq | ast::BinaryOp::Ne => {
                    if !a_ty.matches(&b_ty, resolved, notes) {
                        return Err(CompilerError::MismatchedTypes {
                            pos: expr.pos.clone(),
                            lhs: a.pos.clone(),
                            lhs_ty: a_ty.pretty_name(&notes.project_tree),
                            rhs: b.pos.clone(),
                            rhs_ty: b_ty.pretty_name(&notes.project_tree),
                            file: notes.current_file.clone()
                        });
                    }
                    ResolvedTy::Bool
                },

                ast::BinaryOp::Gt | ast::BinaryOp::Ge |
                ast::BinaryOp::Lt | ast::BinaryOp::Le => {
                    expect_two_numbers(expr, a, &a_ty, b, &b_ty, resolved, notes)?;
                    ResolvedTy::Bool
                }

                ast::BinaryOp::Sub | ast::BinaryOp::Div |
                ast::BinaryOp::Mul | ast::BinaryOp::Mod |
                ast::BinaryOp::Pow => {
                    expect_two_numbers(expr, a, &a_ty, b, &b_ty, resolved, notes)?;
                    ResolvedTy::Number
                }

                ast::BinaryOp::Add => {
                    if a_ty.resolve_to_top(resolved) == ResolvedTy::Text {
                        match b_ty.resolve_to_top(resolved){
                            ResolvedTy::Text | ResolvedTy::Number | ResolvedTy::Bool =>
                                ResolvedTy::Text,
                            _ => {
                                return Err(CompilerError::Custom(
                                    create_error(
                                        error::info::ErrorInfo::from_area(CodeArea {
                                            pos: expr.pos.clone(), file: notes.current_file.clone()
                                        }),
                                        "Compilation error",
                                        &[(
                                            CodeArea { pos: b.pos.clone(), file: notes.current_file.clone() },
                                            &format!(
                                                "'+' operator expects this to be text/number/boolean but got {}.",
                                                b_ty.pretty_name(&notes.project_tree)
                                            ),
                                        )],
                                        None
                                    )
                                ));
                            }
                        }
                    } else {
                        expect_two_numbers(expr, a, &a_ty, b, &b_ty, resolved, notes)?;
                        ResolvedTy::Number
                    }
                }

                ast::BinaryOp::And | ast::BinaryOp::Or => {
                    if a_ty.resolve_to_top(resolved) != ResolvedTy::Bool {
                        return Err(CompilerError::MismatchedTypes {
                            pos: expr.pos.clone(),
                            lhs: a.pos.clone(),
                            lhs_ty: a_ty.pretty_name(&notes.project_tree),
                            rhs: expr.pos.clone(),
                            rhs_ty: "This expression requires both values to be a boolean.".to_string(),
                            file: notes.current_file.clone()
                        });
                    }
                    if b_ty.resolve_to_top(resolved) != ResolvedTy::Bool {
                        return Err(CompilerError::MismatchedTypes {
                            pos: expr.pos.clone(),
                            lhs: expr.pos.clone(),
                            lhs_ty: "This expression requires both values to be a boolean.".to_string(),
                            rhs: b.pos.clone(),
                            rhs_ty: b_ty.pretty_name(&notes.project_tree),
                            file: notes.current_file.clone()
                        });
                    }

                    ResolvedTy::Bool
                }
            };

            resolved.types.insert(expr.id, ty.clone());
            ty
        }

        _ => { notes.default_ret_ty.clone() }
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
    resolved: &mut ResolvedData,
    notes: &ResolutionNotes
) -> Result<ResolvedTy, CompilerError> {
    let mut args = Vec::new();

    for arg in signature.arguments.iter() {
        let ty = resolve_ty(&arg.ty, notes)?;
        args.push(ty.clone());
        resolved.types.insert(arg.id, ty);
    }

    Ok(ResolvedTy::Function(
        args,
        Box::new(
            resolve_ty(&signature.return_type, notes)?
        )
    ))
}
