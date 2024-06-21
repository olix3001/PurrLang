use ahash::HashMapExt;
use common::PurrSource;
use parser::{ast::NodeId, parser::parse_purr};
use resolution::{project_tree::ProjectTree, resolve_types::resolve_types, ResolvedTy};

#[test]
fn resolve_easy_project_types() {
    const SOURCE: &str = "
        mod math {
            struct Vec2 {
                x: number,
                y: number
            }

            def add_vec2(a: Vec2, b: Vec2) -> Vec2 {}
        }
        
        import math::Vec2;

        def sub_vec2(a: Vec2, b: Vec2) -> Vec2 {}
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);
    let resolved_types = resolve_types(&ast, &names).unwrap();

    assert_eq!(resolved_types.types.get(&NodeId::new(5)), Some(
        &ResolvedTy::Function(vec![
            ResolvedTy::Path(NodeId::new(2)),
            ResolvedTy::Path(NodeId::new(2))
        ], Box::new(ResolvedTy::Path(NodeId::new(2))))
    ));

    assert_eq!(resolved_types.types.get(&NodeId::new(10)), Some(
        &ResolvedTy::Function(vec![
            ResolvedTy::Path(NodeId::new(2)),
            ResolvedTy::Path(NodeId::new(2))
        ], Box::new(ResolvedTy::Path(NodeId::new(2))))
    ));

    let mut expected_struct = ahash::HashMap::new();
    expected_struct.insert("x".to_string(), ResolvedTy::Number);
    expected_struct.insert("y".to_string(), ResolvedTy::Number);

    assert_eq!(resolved_types.types.get(&NodeId::new(2)), Some(
        &ResolvedTy::Struct(expected_struct)
    ));
}

