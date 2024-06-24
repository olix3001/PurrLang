use common::PurrSource;
use compiler::compile_purr;
use parser::parser::parse_purr;
use resolution::{project_tree::ProjectTree, resolve::resolve};


#[test]
fn compile_empty_green_flag() {
    const SOURCE: &str = "
        @green_flag {}
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let resolved = resolve(&ast, &names).unwrap();

    let code = compile_purr(
        &ast.0,
        PurrSource::Unknown,
        &resolved
    ).unwrap();

    assert_eq!(code.blocks.len(), 1);
    let first_block = code.blocks.values().next().unwrap();
    assert_eq!(first_block.opcode, "event_whenflagclicked");
    assert_eq!(first_block.top_level, true);
}
