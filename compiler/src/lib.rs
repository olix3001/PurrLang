use codegen::{DataId, blocks::{Sb3Code, BlocksBuilder}};
use common::{FileRange, PurrSource};
use error::{create_error, info::CodeArea, CompilerError};
use parser::ast;
use resolution::resolve::ResolvedData;

#[derive(Debug, Clone)]
pub struct CompileNotes<'a> {
    current_file: PurrSource,
    resolved_data: &'a ResolvedData
}

pub fn compile_purr(
    items: &Vec<ast::Item>,
    source: PurrSource,
    resolved_data: &ResolvedData
) -> Result<Sb3Code, CompilerError> {
    let mut notes = CompileNotes {
        current_file: source,
        resolved_data
    };
    let mut code = Sb3Code::default();

    compile_items(
        items,
        &mut BlocksBuilder::new(&mut code),
        &mut notes
    )?;

    Ok(code)
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
    notes: &CompileNotes
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
    Ok(block.finish())
}
