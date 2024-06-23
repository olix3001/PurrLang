use ahash::HashMapExt;
use common::PurrSource;
use parser::{ast::NodeId, parser::parse_purr};
use resolution::{project_tree::ProjectTree, resolve::resolve, ResolvedTy};

#[test]
fn resolve_project_types() {
    const SOURCE: &str = "
        mod math {
            struct Vec2 {
                x: number,
                y: number
            }

            def add_vec2(a: Vec2, b: Vec2) -> Vec2 {
                let a = 1;
                let b = a;
                Vec2 { x: a, y: b }
            }
        }
        
        import math::Vec2;

        def sub_vec2(a: Vec2, b: Vec2) -> Vec2 {
            Vec2 { x: 1, y: 1 }
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let resolved = resolve(&ast, &names).unwrap();
    // panic!("{resolved:?}");

    assert_eq!(resolved.types.get(&NodeId::new(15)), Some(
        &ResolvedTy::Function(vec![
            ResolvedTy::Path(NodeId::new(2)),
            ResolvedTy::Path(NodeId::new(2))
        ], Box::new(ResolvedTy::Path(NodeId::new(2))))
    ));

    assert_eq!(resolved.types.get(&NodeId::new(26)), Some(
        &ResolvedTy::Function(vec![
            ResolvedTy::Path(NodeId::new(2)),
            ResolvedTy::Path(NodeId::new(2))
        ], Box::new(ResolvedTy::Path(NodeId::new(2))))
    ));

    assert_eq!(resolved.types.get(&NodeId::new(6)), Some(
        &ResolvedTy::Number
    ));
    assert_eq!(resolved.types.get(&NodeId::new(8)), Some(
        &ResolvedTy::Number
    ));
    assert_eq!(resolved.variables.get(&NodeId::new(7)), Some(
        &NodeId::new(6)
    ));

    let mut expected_struct = ahash::HashMap::new();
    expected_struct.insert("x".to_string(), ResolvedTy::Number);
    expected_struct.insert("y".to_string(), ResolvedTy::Number);

    assert_eq!(resolved.types.get(&NodeId::new(2)), Some(
        &ResolvedTy::Struct(expected_struct)
    ));
}

#[test]
#[should_panic]
fn resolve_field_names_unknown() {
    const SOURCE: &str = "
        struct Hello {
            x: number
        }

        def world() -> number {
            let a = Hello { x: 1 };
            a.y
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let _resolved = resolve(&ast, &names).unwrap();
}

#[test]
#[should_panic]
fn resolve_missing_field() {
    const SOURCE: &str = "
        struct Hello {
            x: number
        }

        def world() -> number {
            let a = Hello { x: 1, y: true };
            a.x
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let _resolved = resolve(&ast, &names).unwrap();
}

#[test]
fn resolve_field_types() {
    const SOURCE: &str = "
        struct Hello {
            x: number
        }

        def world() -> number {
            let a = Hello { x: 1 };
            a.x
        }
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let _resolved = resolve(&ast, &names).unwrap();
}
