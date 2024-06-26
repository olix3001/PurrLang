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

        def greet(user: text) {
            say(\"Hello World\");
        }

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

    panic!("code: \n\n{}\n\n", build_project_json(&code).unwrap());

    assert_eq!(code.blocks.len(), 3);
    assert_eq!(code.variables.len(), 1);
}
