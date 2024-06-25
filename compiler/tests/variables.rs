use codegen::{build_project_json, DataId};
use common::PurrSource;
use compiler::compile_purr;
use parser::parser::parse_purr;
use resolution::{project_tree::ProjectTree, resolve::resolve};

#[test]
fn compile_single_variable() {
    const SOURCE: &str = "
        block say(MESSAGE: text) looks_say {
            inputs: .{ MESSAGE }
        }

        @green_flag {
            let message = \"Hello, World!\";
            say(message);
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let resolved = resolve(&ast, &names).unwrap();

    let code = compile_purr(
        &ast.0,
        PurrSource::Unknown,
        &resolved
    ).unwrap();


    assert_eq!(code.blocks.len(), 3);
    assert_eq!(code.variables.len(), 1);

    let first_block = code.blocks.get(&DataId::from_numeric_id(0)).unwrap();
    assert_eq!(first_block.opcode, "event_whenflagclicked");
    assert_eq!(first_block.top_level, true);

    let second_block = code.blocks.get(&DataId::from_numeric_id(2)).unwrap();
    assert_eq!(second_block.opcode, "data_setvariableto");
    assert_eq!(second_block.inputs.len(), 1);
    assert_eq!(first_block.next, Some(DataId::from_numeric_id(2)));
    assert_eq!(second_block.parent, Some(DataId::from_numeric_id(0)));
}
