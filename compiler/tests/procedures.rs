use codegen::{build_project_json, DataId};
use common::PurrSource;
use compiler::compile_purr;
use parser::parser::parse_purr;
use resolution::{project_tree::ProjectTree, resolve::resolve};

#[test]
fn compile_single_argument_procedure() {
    const SOURCE: &str = "
        block say(MESSAGE: text) looks_say {
            inputs: .{ MESSAGE }
        }

        def greet(user: text) {
            say(user);
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

    assert_eq!(code.blocks.len(), 6);
}

#[test]
fn compile_struct_argument_procedure() {
    const SOURCE: &str = "
        block say(MESSAGE: text) looks_say {
            inputs: .{ MESSAGE }
        }

        struct User { name: text, nick: text }

        def greet(user: User) {
            say(user.name);
        }

        @green_flag {
            greet(.{ name: \"Hello\", nick: \"World\" });
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

    assert_eq!(code.blocks.len(), 8);
    // TODO: Check correctness of ID's
}
