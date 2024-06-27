use ahash::{HashMap, HashMapExt};
use codegen::{blocks::{ArgumentTy, BlocksBuilder, Sb3Code, Sb3FunctionDefinition}, DataId};
use common::{FileRange, PurrSource};
use error::{create_error, info::CodeArea, CompilerError};
use parser::ast::{self, NodeId};
use resolution::{resolve::ResolvedData, ResolvedTy};
use value::Value;

pub mod value;

#[derive(Debug, Clone)]
pub struct CompileNotes<'a> {
    current_file: PurrSource,
    resolved_data: &'a ResolvedData,
    variables: HashMap<NodeId, Value>,
    proc_definitions: HashMap<NodeId, Sb3FunctionDefinition>,
    proc_arguments: HashMap<NodeId, Value>,
    proc_definition_ids: HashMap<NodeId, DataId>,
    proc_returns: HashMap<NodeId, Value>,
    current_proc: Option<NodeId>
}

pub fn compile_purr(
    items: &Vec<ast::Item>,
    source: PurrSource,
    resolved_data: &ResolvedData
) -> Result<Sb3Code, CompilerError> {
    let mut notes = CompileNotes {
        current_file: source,
        resolved_data,
        variables: HashMap::new(),
        proc_definitions: HashMap::new(),
        proc_arguments: HashMap::new(),
        proc_definition_ids: HashMap::new(),
        proc_returns: HashMap::new(),
        current_proc: None
    };
    let mut builder = BlocksBuilder::new();

    compile_items_prepass(
        items,
        &mut builder,
        &mut notes
    )?;
    compile_items(
        items,
        &mut builder,
        &mut notes
    )?;

    Ok(builder.finish().unwrap())
}

pub fn compile_items_prepass(
    items: &Vec<ast::Item>,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<(), CompilerError> {
    for item in items.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                let temp_file = notes.current_file.clone();
                notes.current_file = module.source.clone();
                compile_items(&module.body, builder, notes)?;
                notes.current_file = temp_file;
            }

            ast::ItemKind::FunctionDefinition(definition) => {
                let mut function_args = Vec::new();
                for arg in definition.signature.arguments.iter() {
                    let ty = notes.resolved_data.types.get(&arg.id).unwrap();
                    let size = ty.size(&notes.resolved_data);
                    let flat_ty = ty.flatten(&notes.resolved_data);

                    for i in 0..size {
                        function_args.push((
                            format!("{}:{i}@{:04x}", arg.name, item.id.num()),
                            if flat_ty[i] == ResolvedTy::Bool { ArgumentTy::Boolean }
                            else { ArgumentTy::TextOrNumber }
                        ));
                    }
                }
                let ids = get_function_arguments(
                    item.id,
                    notes
                );
                let (sb, def) = builder.define_function(
                    &definition.name,
                    function_args.as_slice(),
                    ids.as_slice(),
                    item.attributes.tags.iter().any(|tag|
                        tag == &ast::AttributeTag::Marker("warp".to_string())
                    )
                );

                let mut current_args = HashMap::new();
                let argument_flat = function_args.iter()
                    .map(|(arg, arg_ty)| Value::Argument(arg.clone(), *arg_ty == ArgumentTy::Boolean))
                    .collect::<Vec<_>>();
                let mut argument_flat_iter = argument_flat.into_iter();
                for arg in definition.signature.arguments.iter() {
                    let ty = notes.resolved_data.types.get(&arg.id).unwrap();

                    current_args.insert(
                        arg.id,
                        Value::from_flat_and_ty(
                            &ty, &mut argument_flat_iter,
                            &notes.resolved_data
                        )
                    );
                }
                notes.proc_arguments.extend(current_args.clone().into_iter());

                notes.proc_definitions.insert(item.id, def);
                notes.proc_definition_ids.insert(item.id, sb.previous());
                let return_value = get_proc_return(item.id, builder, notes)?;
                notes.proc_returns.insert(item.id, return_value);
            }

            _ => {}
        }
    }
    Ok(())
}

pub fn compile_items(
    items: &Vec<ast::Item>,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<(), CompilerError> {
    for item in items.iter() {
        match &item.kind {
            ast::ItemKind::Module(module) => {
                let temp_file = notes.current_file.clone();
                notes.current_file = module.source.clone();
                compile_items(&module.body, builder, notes)?;
                notes.current_file = temp_file;
            }
            ast::ItemKind::Trigger(trigger) => {
                compile_trigger(
                    trigger,
                    item.pos.clone(),
                    builder,
                    notes
                )?;
            }
            ast::ItemKind::FunctionDefinition(definition) => {
                let definition_id = notes.proc_definition_ids.get(&item.id)
                    .unwrap().clone();
                let mut subbuilder = builder.subbuilder_for(&definition_id);
                notes.current_proc = Some(item.id);
                compile_statements(
                    &definition.body,
                    &mut subbuilder,
                    notes
                )?;
                notes.current_proc = None;
            }
            _ => {}
        }
    }
    Ok(())
}

fn get_proc_return(
    node_id: NodeId,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<Value, CompilerError> {
    if let Some(value) = notes.proc_returns.get(&node_id) {
        return Ok(value.clone());
    }

    let f_name = notes.resolved_data.names.get(&node_id).unwrap().clone();
    let f_ty = notes.resolved_data.types.get(&node_id).unwrap().clone();
    let ResolvedTy::Function(_, ret_ty) = &f_ty else { unreachable!() };
    let value = define_variables_for_type(
        &format!("ret:{f_name}"),
        node_id,
        ret_ty,
        builder,
        notes
    )?;
    notes.proc_returns.insert(node_id, value.clone());
    Ok(value)
}

fn get_function_arguments(
    node_id: NodeId,
    notes: &mut CompileNotes
) -> Vec<DataId> {
    if let Some(def) = notes.proc_definitions.get(&node_id) {
        return def.arguments.clone();
    }
    let mut ids = Vec::new();
    let ty = notes.resolved_data.types.get(&node_id).unwrap();
    let ResolvedTy::Function(args, _) = ty 
        else { unreachable!("There is error in the compiler?") };
    for arg in args.iter() {
        for _ in 0..arg.size(&notes.resolved_data) {
            let id = DataId::new();
            ids.push(id);
        }
    }
    notes.proc_definitions.insert(
        node_id, Sb3FunctionDefinition {
            arguments: ids.clone(),
            ..Default::default()
        }
    );

    ids
}

pub fn compile_trigger(
    trigger: &ast::Trigger,
    pos: FileRange,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<DataId, CompilerError> {
    let opcode = match trigger.name.as_str() {
        "green_flag" => "event_whenflagclicked",

        _ => return Err(CompilerError::Custom(
            create_error(
                error::info::ErrorInfo::from_area(CodeArea {
                    pos: pos.clone(), file: notes.current_file.clone()
                }),
                "Compilation error",
                &[(
                    CodeArea { pos, file: notes.current_file.clone() },
                    &format!("Could not find trigger with name '{}'.", trigger.name),
                )],
                None
            )
        ))
    };
    let mut block = builder.block(opcode);
    block.top_level();
    let block = block.finish();

    compile_statements(&trigger.body, builder, notes)?;

    Ok(block)
}

pub fn compile_statements(
    stmts: &Vec<ast::Statement>,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<(), CompilerError> {
    for stmt in stmts.iter() {
        match &stmt.kind {
            ast::StatementKind::Expr(expr) => {
                compile_expr(expr, builder, notes)?;
            }
            ast::StatementKind::LetDefinition(definition) => {
                let ty = notes.resolved_data.types.get(&stmt.id).unwrap();
                let variable = define_variables_for_type(
                    &definition.symbol,
                    stmt.id,
                    ty,
                    builder,
                    notes
                )?;
                notes.variables.insert(stmt.id, variable);
                if let Some(value) = &definition.value {
                    let value = compile_expr(value, builder, notes)?;
                    write_variable(
                        &notes.variables.get(&stmt.id).unwrap().clone(),
                        value,
                        builder,
                        notes
                    )?;
                }
            }
            _ => todo!()
        }
    }
    Ok(())
}

fn write_variable(
    variable: &Value,
    value: Value,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes,
) -> Result<(), CompilerError> {
    match &variable {
        Value::Variable(_) => {
            let mut b = builder.block("data_setvariableto");
            b.field(
                "VARIABLE", 
                variable.as_sb3_field(builder)?
            );
            b.input(
                "VALUE",
                &[value.into_sb3(builder, b.id())?]
            );
        },
        Value::Struct(target) => {
            let Value::Struct(source) = value else {
                panic!("Only struct can be assigned to struct");
            };
            for (field_name, field_value) in target.iter() {
                write_variable(
                    field_value,
                    source.get(field_name).unwrap().clone(),
                    builder,
                    notes,
                )?;
            }
        }
        _ => panic!("value {value:?} is not writable!")
    }
    Ok(())
}

fn define_variables_for_type(
    prefix: &str,
    node_id: NodeId,
    ty: &ResolvedTy,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<Value, CompilerError> {
    let ty = ty.resolve_to_top(&notes.resolved_data);
    match ty {
        ResolvedTy::Text | ResolvedTy::Number | ResolvedTy::Bool => {
            let id = builder.define_variable(
                format!("{prefix}@{:04x}", node_id.num())
            );
            Ok(Value::Variable(id))
        }
        
        ResolvedTy::Struct(struct_) => {
            let mut values = HashMap::new();
            for (field_name, field_value) in struct_.iter() {
                let value = define_variables_for_type(
                    &format!("{prefix}.{field_name}"),
                    node_id,
                    &field_value,
                    builder,
                    notes
                )?; 

                values.insert(field_name.to_string(), value);
            }

            Ok(Value::Struct(values))
        }

        _ => Ok(Value::Empty)
    }
}

pub fn compile_expr(
    expr: &ast::Expression,
    builder: &mut BlocksBuilder,
    notes: &mut CompileNotes
) -> Result<Value, CompilerError> {
    match &expr.kind {
        ast::ExpressionKind::String(text) =>
            Ok(Value::Text(text.clone())),
        ast::ExpressionKind::Number(number) =>
            Ok(Value::Number(*number)),

        ast::ExpressionKind::Path(_) => {
            // Could be path to block/function.
            if let Some(ResolvedTy::Path(id)) = notes.resolved_data.types.get(
                &expr.id
            ) {
                if let Some(block) = notes.resolved_data.blocks.get(id) {
                    return Ok(Value::BlockRef(block.clone()));
                }
                if notes.proc_definitions.contains_key(id) {
                    return Ok(Value::FunctionRef(*id))
                }
            }
 
            if let Some(def_id) = notes.resolved_data.variables.get(&expr.id) {
                if let Some(variable) = notes.variables.get(def_id) {
                    return Ok(variable.clone());
                }

                if notes.current_proc.is_some() {
                    if let Some(argument) = notes.proc_arguments.get(def_id) {
                        return Ok(argument.clone());
                    }
                }
            }

            panic!("Path resolution went wrong... sorry :c");
        }

        ast::ExpressionKind::Field(obj, field) => {
            let obj = compile_expr(&*obj, builder, notes)?;
            let Value::Struct(obj) = obj else {
                panic!("Field access is only allowed on structs");
            };

            Ok(obj.get(field).unwrap().clone())
        }

        ast::ExpressionKind::Call { callee, generics: _generics, arguments } => {
            let callee_val = compile_expr(callee, builder, notes)?;

            match callee_val {
                Value::BlockRef(block) => {
                    let mut b = builder.block(&block.opcode);

                    for (i, arg) in arguments.iter().enumerate() {
                        let arg = compile_expr(arg, builder, notes)?;
                        b.input(
                            &block.inputs[i],
                            &[arg.into_sb3(builder, b.id())?]
                        );
                    }

                    Ok(Value::BlockCall(b.finish()))
                },
                _ => return Err(CompilerError::Custom(
                    create_error(
                        error::info::ErrorInfo::from_area(CodeArea {
                            pos: callee.pos.clone(), file: notes.current_file.clone()
                        }),
                        "Compilation error",
                        &[(
                            CodeArea { pos: callee.pos.clone(), file: notes.current_file.clone() },
                            "This value is not callable."
                        )],
                        None
                    )
                ))
            }
        },

        ast::ExpressionKind::StructLiteral(_, fields) |
        ast::ExpressionKind::AnonStruct(fields) => {
            let mut value = HashMap::new();
            for field in fields.iter() {
                value.insert(
                    field.name.clone(),
                    compile_expr(&field.value, builder, notes)?
                );
            }
            Ok(Value::Struct(value))
        }

        _ => todo!()
    }
}
