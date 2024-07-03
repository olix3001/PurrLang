use std::collections::HashSet;

use ahash::{HashMap, HashMapExt};
use codegen::{blocks::{ArgumentTy, BlocksBuilder, Mutation, Sb3Code, Sb3Field, Sb3FunctionDefinition, Sb3Value}, DataId};
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
    current_proc: Option<NodeId>,
    items_to_skip: HashSet<NodeId>
}

pub fn compile_purr(
    items: &Vec<ast::Item>,
    source: PurrSource,
    resolved_data: &ResolvedData,
    items_to_skip: HashSet<NodeId>
) -> Result<Sb3Code, CompilerError> {
    let mut notes = CompileNotes {
        current_file: source,
        resolved_data,
        variables: HashMap::new(),
        proc_definitions: HashMap::new(),
        proc_arguments: HashMap::new(),
        proc_definition_ids: HashMap::new(),
        proc_returns: HashMap::new(),
        current_proc: None,
        items_to_skip
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
        if notes.items_to_skip.contains(&item.id) { continue; }
        match &item.kind {
            ast::ItemKind::Module(module) => {
                let temp_file = notes.current_file.clone();
                notes.current_file = module.source.clone();
                compile_items_prepass(&module.body, builder, notes)?;
                notes.current_file = temp_file;
            }

            ast::ItemKind::FunctionDefinition(definition) => {
                let mut function_args = Vec::new();
                for arg in definition.signature.arguments.iter() {
                    let ty = notes.resolved_data.types.get(&arg.id).unwrap();
                    let size = ty.size(notes.resolved_data);
                    let flat_ty = ty.flatten(notes.resolved_data);

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
                builder.reset_previous();
                let (di, def) = builder.define_function(
                    &format!(
                        "{}@{:04x}", 
                        definition.name,
                        item.id.num()
                    ),
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
                            ty, &mut argument_flat_iter,
                            notes.resolved_data
                        )
                    );
                }
                notes.proc_arguments.extend(current_args.clone().into_iter());

                notes.proc_definitions.insert(item.id, def);
                notes.proc_definition_ids.insert(item.id, di);
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
        if notes.items_to_skip.contains(&item.id) { continue; }
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
        for _ in 0..arg.size(notes.resolved_data) {
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
        "key_pressed" => "event_whenkeypressed",
        "sprite_clicked" => "event_whenthisspriteclicked",
        "backdrop_switch" => "event_whenbackdropswitchesto",

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
    builder.reset_previous();
    let mut block = builder.block(opcode);
    block.top_level();

    match trigger.name.as_str() {
        "key_pressed" => {
            let ast::ExpressionKind::String(key) = &trigger.arguments.first().unwrap().kind
                else { panic!("Expected string literal for key in @key_pressed. ")};
            block.field("KEY_OPTION", Sb3Field::Argument(key.to_string()));
        }
        "backdrop_switch" => {
            let ast::ExpressionKind::String(bd) = &trigger.arguments.first().unwrap().kind
                else { panic!("Expected string literal for key in @backdrop_switch. ")};
            block.field("BACKDROP", Sb3Field::Argument(bd.to_string()));
        }
        _ => {}
    }

    let mut subbuilder = builder.subbuilder_for(block.id());
    compile_statements(&trigger.body, &mut subbuilder, notes)?;

    {
        let block = block.block.as_mut().unwrap();
        block.next = subbuilder.first();
    }

    let block = block.finish();

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
            ast::StatementKind::ExprNoSemi(expr) => {
                let Some(current_proc) = notes.current_proc
                else { panic!("Temporary error: Returning expression (no-semicolon) may only be used in procedures")};
                // TODO: Allow if expr type is void.
                
                write_variable(
                    &notes.proc_returns.get(&current_proc).unwrap().clone(),
                    compile_expr(expr, builder, notes)?,
                    builder,
                    notes
                )?;
            }
            ast::StatementKind::Return(value) => {
                if let Some(value) = value {
                    let Some(current_proc) = notes.current_proc 
                    else { panic!("Temporary error: return with value may only be used in procedures.") }; 
                    write_variable(
                        &notes.proc_returns.get(&current_proc).unwrap().clone(),
                        compile_expr(value, builder, notes)?,
                        builder,
                        notes
                    )?;

                    let mut stop = builder.block("control_stop");
                    stop.block.as_mut().unwrap().mutation = Some(Mutation::no_next());
                    stop.field("STOP_OPTION", Sb3Field::Argument("this script".to_string()));
                    return Ok(());
                }

                let mut stop = builder.block("control_stop");
                stop.block.as_mut().unwrap().mutation = Some(Mutation::no_next());
                stop.field("STOP_OPTION", Sb3Field::Argument("this script".to_string()));
                return Ok(());
            }

            ast::StatementKind::Conditional(condition, if_true, if_false) => {
                let mut b = builder.block(
                    if if_false.is_some() { "control_if_else" }
                    else { "control_if" }
                );

                let condition = compile_expr(condition, builder, notes)?;
                b.input("CONDITION", &[condition.into_sb3(builder, b.id())?]);

                let mut substack_builder = builder.subbuilder_for(b.id());
                compile_statements(if_true, &mut substack_builder, notes)?;
                if let Some(first) = substack_builder.first() {
                    b.input("SUBSTACK", &[Sb3Value::Ptr(first)]);
                }

                if let Some(if_false) = &if_false {
                    let mut substack_builder = builder.subbuilder_for(b.id());
                    compile_statements(if_false, &mut substack_builder, notes)?;
                    if let Some(first) = substack_builder.first() {
                        b.input("SUBSTACK2", &[Sb3Value::Ptr(first)]);
                    }
                }
            }

            ast::StatementKind::Repeat(count, body) => {
                let mut b = builder.block("control_repeat");

                let count = compile_expr(count, builder, notes)?;
                b.input("TIMES", &[count.into_sb3(builder, b.id())?]);

                let mut substack_builder = builder.subbuilder_for(b.id());
                compile_statements(body, &mut substack_builder, notes)?;
                if let Some(first) = substack_builder.first() {
                    b.input("SUBSTACK", &[Sb3Value::Ptr(first)]);
                }
            }

            ast::StatementKind::While(condition, body) => {
                let mut b = builder.block("control_repeat_until");

                let condition = compile_expr(condition, builder, notes)?;
                // Repeat until works opposite to a while for some reason.
                let condition = {
                    let mut b = builder.block("operator_not");
                    b.input("OPERAND", &[condition.into_sb3(builder, b.id())?]);
                    Value::BlockCall(b.finish())
                };

                b.input("CONDITION", &[condition.into_sb3(builder, b.id())?]);

                let mut substack_builder = builder.subbuilder_for(b.id());
                compile_statements(body, &mut substack_builder, notes)?;
                if let Some(first) = substack_builder.first() {
                    b.input("SUBSTACK", &[Sb3Value::Ptr(first)]);
                }
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
        _ => {}
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
    let ty = ty.resolve_to_top(notes.resolved_data);
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
                    field_value,
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
        ast::ExpressionKind::Bool(value) => {
            let mut b = builder.block("operator_equals");
            // Workaround as scratch does not support true/false.
            b.input("OPERAND1", &[
                Sb3Value::Number(if *value { 1. } else { 0. })
            ]);
            b.input("OPERAND2", &[Sb3Value::Number(1.)]);
            Ok(Value::BlockCall(b.finish()))
        }

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

        ast::ExpressionKind::TypeCast(cast_expr, _) => 
            compile_expr(cast_expr, builder, notes),

        ast::ExpressionKind::Assignment(subject, value) => {
            let subject_value = compile_expr(subject, builder, notes)?;
            let value_value = compile_expr(value, builder, notes)?;

            write_variable(&subject_value, value_value, builder, notes)?;
            Ok(subject_value)
        }

        ast::ExpressionKind::Field(obj, field) => {
            let obj = compile_expr(obj, builder, notes)?;
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

                    for (field_name, field_value) in block.fields.iter() {
                        b.field(
                            field_name,
                            Sb3Field::Argument(field_value.clone())
                        );
                    }

                    Ok(Value::BlockCall(b.finish()))
                },
                Value::FunctionRef(id) => {
                    let mut flat_args = Vec::new();
                    for arg in arguments.iter() {
                        let arg = compile_expr(arg, builder, notes)?;
                        flat_args.extend(arg.flatten());
                    }

                    let Some(definition) = notes.proc_definitions.get(&id)
                        else { unreachable!() };

                    call_function(builder, definition, flat_args.as_slice())?;

                    // If calls are chained return values can override each other.
                    let ret_ty = notes.resolved_data.types.get(&expr.id).unwrap();
                    let call_vars = define_variables_for_type(
                        "call:",
                        expr.id,
                        ret_ty,
                        builder,
                        notes
                    )?;
                    
                    let proc_ret = get_proc_return(id, builder, notes)?;
                    write_variable(&call_vars, proc_ret, builder, notes)?;
                    Ok(call_vars)
                },
                _ => Err(CompilerError::Custom(
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

        ast::ExpressionKind::Unary(op, expr) => {
            let expr = compile_expr(expr, builder, notes)?;
            match op {
                ast::UnaryOp::Neg => {
                    let mut b = builder.block("operator_subtract");
                    b.input("NUM1", &[Sb3Value::Number(0.)]);
                    b.input("NUM2", &[expr.into_sb3(builder, b.id())?]);
                    Ok(Value::BlockCall(b.finish()))
                },
                ast::UnaryOp::Not => {
                    let mut b = builder.block("operator_not");
                    b.input("OPERAND", &[expr.into_sb3(builder, b.id())?]);
                    Ok(Value::BlockCall(b.finish()))
                }
            }
        }

        ast::ExpressionKind::Binary(a, op, b) => {
            let ac = compile_expr(a, builder, notes)?;
            let bc = compile_expr(b, builder, notes)?;

            match op {
                ast::BinaryOp::Lt | ast::BinaryOp::Le | 
                ast::BinaryOp::Gt | ast::BinaryOp::Ge |
                ast::BinaryOp::And | ast::BinaryOp::Or => {
                    let mut b = builder.block(match op {
                        ast::BinaryOp::Lt => "operator_lt",
                        ast::BinaryOp::Le => "operator_le",
                        ast::BinaryOp::Gt => "operator_gt",
                        ast::BinaryOp::Ge => "operator_ge",
                        ast::BinaryOp::And => "operator_and",
                        ast::BinaryOp::Or => "operator_or",
                        _ => unreachable!()
                    });

                    b.input("OPERAND1", &[
                        ac.into_sb3(builder, b.id())?
                    ]);
                    b.input("OPERAND2", &[
                        bc.into_sb3(builder, b.id())?
                    ]);

                    Ok(Value::BlockCall(b.finish()))
                }

                ast::BinaryOp::Sub | ast::BinaryOp::Mul |
                ast::BinaryOp::Div | ast::BinaryOp::Mod => {
                    let mut b = builder.block(match op {
                        ast::BinaryOp::Sub => "operator_subtract",
                        ast::BinaryOp::Mul => "operator_multiply",
                        ast::BinaryOp::Div => "operator_divide",
                        ast::BinaryOp::Mod => "operator_mod",
                        _ => unreachable!()
                    });

                    b.input("NUM1", &[
                        ac.into_sb3(builder, b.id())?
                    ]);
                    b.input("NUM2", &[
                        bc.into_sb3(builder, b.id())?
                    ]);

                    Ok(Value::BlockCall(b.finish()))
                }

                ast::BinaryOp::Add => {
                    let ty = notes.resolved_data.types.get(&expr.id).unwrap();

                    if *ty == ResolvedTy::Text {
                        let mut b = builder.block("operator_join");
                        b.input("STRING1", &[ac.into_sb3(builder, b.id())?]);
                        b.input("STRING2", &[bc.into_sb3(builder, b.id())?]);
                        Ok(Value::BlockCall(b.finish()))
                    } else {
                        let mut b = builder.block("operator_add");
                        b.input("NUM1", &[ac.into_sb3(builder, b.id())?]);
                        b.input("NUM2", &[bc.into_sb3(builder, b.id())?]);
                        Ok(Value::BlockCall(b.finish()))
                    }
                }

                ast::BinaryOp::Eq => {
                    deep_equals(ac, bc, builder, notes)
                }

                ast::BinaryOp::Ne => {
                    let eq = deep_equals(ac, bc, builder, notes)?;
                    let mut not = builder.block("operator_not");
                    not.input("OPERAND", &[eq.into_sb3(builder, not.id())?]);
                    Ok(Value::BlockCall(not.finish()))
                }

                ast::BinaryOp::Pow => { unimplemented!("Power operator is not implemented yet.") }
            }
        }

        _ => todo!()
    }
}

/// Compares complex values including structs.
fn deep_equals(
    a: Value,
    b: Value,
    builder: &mut BlocksBuilder,
    _notes: &mut CompileNotes
) -> Result<Value, CompilerError> {
    match a {
        Value::Struct(_) => {
            let a = a.flatten();
            let b = b.flatten();

            a.into_iter().zip(b).try_fold(Value::Empty, |acc, (a, b)| {
                let mut cmp = builder.block("operator_equals");
                cmp.input("OPERAND1", &[a.into_sb3(builder, cmp.id())?]);
                cmp.input("OPERAND2", &[b.into_sb3(builder, cmp.id())?]);
                let cmp = Value::BlockCall(cmp.finish());
                if acc == Value::Empty {
                    Ok(cmp)
                } else {
                    let mut and = builder.block("operator_and");
                    and.input("OPERAND1", &[acc.into_sb3(builder, and.id())?]);
                    and.input("OPERAND2", &[cmp.into_sb3(builder, and.id())?]);
                    Ok(Value::BlockCall(and.finish()))
                }
            })
        },
        _ => {
            let mut eq = builder.block("operator_equals");
            eq.input("OPERAND1", &[a.into_sb3(builder, eq.id())?]);
            eq.input("OPERAND2", &[b.into_sb3(builder, eq.id())?]);
            Ok(Value::BlockCall(eq.finish()))
        }
    }
}

fn call_function(
    builder: &mut BlocksBuilder,
    definition: &Sb3FunctionDefinition,
    arguments: &[Value]
) -> Result<(), CompilerError> {
    let mut call = builder.block("procedures_call");
    for (arg_id, arg) in definition.arguments.iter().zip(arguments.iter()) {
        call.input(&arg_id.0, &[arg.clone().into_sb3(builder, call.id())?]);
    }

    {
        let call = call.block.as_mut().unwrap();
        call.mutation = Some(Mutation::default());
        let mutation = call.mutation.as_mut().unwrap();
        mutation.warp = definition.warp;
        mutation.proccode = definition.proccode.clone();
        mutation.argumentids = definition.arguments.iter()
            .map(|arg| arg.0.clone()).collect();
    }

    Ok(())
}
