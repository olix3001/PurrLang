use ahash::{HashMap, HashMapExt};
use codegen::{DataId, blocks::{Sb3Code, BlocksBuilder}};
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
    variables: HashMap<NodeId, Value>
}

pub fn compile_purr(
    items: &Vec<ast::Item>,
    source: PurrSource,
    resolved_data: &ResolvedData
) -> Result<Sb3Code, CompilerError> {
    let mut notes = CompileNotes {
        current_file: source,
        resolved_data,
        variables: HashMap::new()
    };
    let mut builder = BlocksBuilder::new();

    compile_items(
        items,
        &mut builder,
        &mut notes
    )?;

    Ok(builder.finish().unwrap())
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
            _ => {}
        }
    }
    Ok(())
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
                value.should_shadow(),
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

        _ => {Ok(Value::Empty)}
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
                // TODO: FunctionRef.
            }
 
            if let Some(def_id) = notes.resolved_data.variables.get(&expr.id) {
                if let Some(variable) = notes.variables.get(def_id) {
                    return Ok(variable.clone());
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
                            arg.should_shadow(),
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
