use common::PurrSource;
use compiler::compile_purr;
use parser::parser::parse_purr;
use resolution::{project_tree::ProjectTree, resolve::resolve};

#[test]
fn compile_returning_block() {
    const SOURCE: &str = "
        block say(MESSAGE: text) looks_say {
            inputs: .{ MESSAGE }
        }
        block ask(QUESTION: text) sensing_askandwait {
            inputs: .{ QUESTION }
        }
        block answer() -> text sensing_answer {}

        @green_flag {
            ask(\"Enter text:\");
            say(answer());
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0, Default::default());
    let resolved = resolve(&ast, &names).unwrap();

    let code = compile_purr(
        &ast.0,
        PurrSource::Unknown,
        &resolved,
        Default::default()
    ).unwrap();

    assert_eq!(code.blocks.len(), 4);
    assert_eq!(code.variables.len(), 0);
}

#[test]
fn compile_unary_operators() {
    const SOURCE: &str = "
        @green_flag {
            let a = 1;
            let b = -a;
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0, Default::default());
    let resolved = resolve(&ast, &names).unwrap();

    let code = compile_purr(
        &ast.0,
        PurrSource::Unknown,
        &resolved,
        Default::default()
    ).unwrap();

    assert_eq!(code.blocks.len(), 4);
    assert_eq!(code.variables.len(), 2);
}

#[test]
fn compile_binary_operators() {
    const SOURCE: &str = "
        block say(MESSAGE: text) looks_say {
            inputs: .{ MESSAGE }
        }

        struct Vec2 { x: number, y: number }

        @green_flag {
            let a: Vec2 = .{ x: 1, y: 2 };
            let b: Vec2 = .{ x: 1, y: 2 };

            say(\"a == b: \" + (a == b));
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0, Default::default());
    let resolved = resolve(&ast, &names).unwrap();

    let code = compile_purr(
        &ast.0,
        PurrSource::Unknown,
        &resolved,
        Default::default()
    ).unwrap();

    assert_eq!(code.blocks.len(), 10);
    assert_eq!(code.variables.len(), 4);
}
