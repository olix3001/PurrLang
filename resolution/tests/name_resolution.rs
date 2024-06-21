use common::PurrSource;
use parser::{ast::NodeId, parser::parse_purr};
use resolution::project_tree::{ProjectTree, ResolutionPath};

#[test]
fn resolve_project_tree_names() {

    const SOURCE: &str = "
        mod hello {
            import world::{lorem, dolor};

            mod world {
                block lorem() ipsum {}

                struct dolor {}
            }

            def sit() -> void {}
        }

        struct amet {}
    ";

    let ast = parse_purr(SOURCE.to_string(), PurrSource::Unknown).unwrap();
    let names = ProjectTree::build_from_ast(Default::default(), &ast.0);

    assert_eq!(names.resolve_name(&["hello"].as_slice().into()), Some(NodeId::new(5)));
    assert_eq!(names.resolve_name(&["hello", "world"].as_slice().into()), Some(NodeId::new(3)));
    assert_eq!(names.resolve_name(&["hello", "world", "lorem"].as_slice().into()), Some(NodeId::new(1)));
    assert_eq!(names.resolve_name(&["hello", "world", "dolor"].as_slice().into()), Some(NodeId::new(2)));
    assert_eq!(names.resolve_name(&["hello", "sit"].as_slice().into()), Some(NodeId::new(4)));
    assert_eq!(names.resolve_name(&["amet"].as_slice().into()), Some(NodeId::new(6)));

    assert_eq!(names.resolve_tree(&["hello"].as_slice().into()).unwrap().imports.get("dolor"), Some(
        &ResolutionPath::from(["hello", "world", "dolor"].as_slice())
    ));
}

