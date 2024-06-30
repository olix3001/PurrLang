use std::collections::{HashMap, HashSet};

use error::CompilerError;
use parser::ast::{self, NodeId, PurrPath};
use petgraph::{algo::dijkstra, data::Build, graph::{DiGraph, NodeIndex}, visit::NodeRef};
use resolution::{project_tree::{ProjectTree, ResolutionPath}, resolve::ResolvedData, ResolvedTy};

pub(crate) fn build_usage_graph(
    ast: &Vec<ast::Item>,
    resolved: &ResolvedData,
    node_ids: &mut HashMap<NodeId, NodeIndex>,
    project_tree: &ProjectTree,
) -> Result<(NodeId, DiGraph<NodeId, ()>), CompilerError> {
    let mut graph = DiGraph::new();
    let root_id = NodeId::next();
    let ix = graph.add_node(root_id);
    node_ids.insert(root_id, ix);

    add_items_to_graph(ast, resolved, &mut graph, node_ids)?;
    find_usages(ast, resolved, project_tree, &mut graph, root_id, &node_ids)?;

    Ok((root_id, graph))
}

pub(crate) fn add_items_to_graph(
    items: &Vec<ast::Item>,
    resolved: &ResolvedData,
    graph: &mut DiGraph<NodeId, ()>,
    node_ids: &mut HashMap<NodeId, NodeIndex>
) -> Result<(), CompilerError> {
    for item in items.iter() {
        let ix = graph.add_node(item.id);
        node_ids.insert(item.id, ix);
        if let ast::ItemKind::Module(module) = &item.kind {
            add_items_to_graph(&module.body, resolved, graph, node_ids)?;
        }
    }

    Ok(())
}

pub(crate) fn find_usages(
    items: &Vec<ast::Item>,
    resolved: &ResolvedData,
    project_tree: &ProjectTree,
    graph: &mut DiGraph<NodeId, ()>,
    current_id: NodeId,
    node_ids: &HashMap<NodeId, NodeIndex>,
) -> Result<(), CompilerError> {
    for item in items.iter() {
        match &item.kind {
            ast::ItemKind::Trigger(_) |
            ast::ItemKind::Import(_) |
            ast::ItemKind::StructDefinition(_) => {
                graph.update_edge(
                    (*node_ids.get(&current_id).unwrap()).into(),
                    (*node_ids.get(&item.id).unwrap()).into(),
                    ()
                );
            }
            _ => {}
        }
        match &item.kind {
            ast::ItemKind::Module(module) => 
                find_usages(
                    &module.body, 
                    resolved,
                    project_tree.subtrees.get(&module.name).unwrap(), 
                    graph, 
                    item.id,
                    node_ids,
                )?,

            ast::ItemKind::Import(tree) => 
                find_import_tree_usages(
                    tree, 
                    project_tree, 
                    Default::default(), 
                    graph,
                    current_id,
                    node_ids
                ),

            ast::ItemKind::Trigger(trig) => 
                find_usages_in_statements(&trig.body, resolved, project_tree, graph, item.id, node_ids)?,
            ast::ItemKind::FunctionDefinition(definition) =>
                find_usages_in_statements(&definition.body, resolved, project_tree, graph, item.id, node_ids)?,
            _ => {}
        }
    }

    Ok(())
}

fn find_import_tree_usages(
    tree: &ast::ImportTree,
    project_tree: &ProjectTree,
    prefix: ResolutionPath,
    graph: &mut DiGraph<NodeId, ()>,
    current_id: NodeId,
    node_ids: &HashMap<NodeId, NodeIndex>
) {
    let prefix = prefix.join(&tree.prefix.clone().into());

    let mut current = project_tree;
    for seg in prefix.segments.iter() {
        if let Some(current_st) = current.subtrees.get(seg) {
            let id = current.names.get(seg).unwrap();
            graph.update_edge(
                (*node_ids.get(&current_id).unwrap()).into(),
                (*node_ids.get(id).unwrap()).into(),
                ()
            );

            current = current_st;
        }
    }

    match &tree.kind {
        ast::ImportKind::Glob => {}
        ast::ImportKind::Name => {}

        ast::ImportKind::Nested(subtrees) => {
            for st in subtrees.iter() {
                find_import_tree_usages(
                    st, 
                    project_tree, 
                    prefix.clone(), 
                    graph, 
                    current_id, 
                    node_ids
                );
            }
        }
    }
}

pub(crate) fn find_usages_in_statements(
    stmts: &Vec<ast::Statement>,
    resolved: &ResolvedData,
    project_tree: &ProjectTree,
    graph: &mut DiGraph<NodeId, ()>,
    current_id: NodeId,
    node_ids: &HashMap<NodeId, NodeIndex>
) -> Result<(), CompilerError> {
    for stmt in stmts.iter() {
        match &stmt.kind {
            ast::StatementKind::Expr(expr) |
            ast::StatementKind::ExprNoSemi(expr) |
            ast::StatementKind::Return(Some(expr)) => {
                find_usages_in_expression(&expr, resolved, project_tree, graph, current_id, node_ids)?;
            }

            ast::StatementKind::Conditional(cond, if_true, if_false) => {
                find_usages_in_expression(&cond, resolved, project_tree, graph, current_id, node_ids)?;
                find_usages_in_statements(if_true, resolved, project_tree, graph, current_id, node_ids)?;
                if let Some(if_false) = &if_false {
                    find_usages_in_statements(if_false, resolved, project_tree, graph, current_id, node_ids)?;
                }
            }

            ast::StatementKind::Repeat(cc, body) |
            ast::StatementKind::While(cc, body) => {
                find_usages_in_expression(&cc, resolved, project_tree, graph, current_id, node_ids)?;
                find_usages_in_statements(body, resolved, project_tree, graph, current_id, node_ids)?;
            }

            ast::StatementKind::LetDefinition(definition) => {
                if let Some(value) = &definition.value {
                    find_usages_in_expression(value, resolved, project_tree, graph, current_id, node_ids)?;
                }
            }
            _ => {}
        }
    }

    Ok(())
}

pub(crate) fn find_usages_in_expression(
    expr: &ast::Expression,
    resolved: &ResolvedData,
    project_tree: &ProjectTree,
    graph: &mut DiGraph<NodeId, ()>,
    current_id: NodeId,
    node_ids: &HashMap<NodeId, NodeIndex>
) -> Result<(), CompilerError> {
    match &expr.kind {
        ast::ExpressionKind::Path(_) => {
            if let Some(ResolvedTy::Path(id)) = resolved.types.get(&expr.id) {
                graph.update_edge(
                    (*node_ids.get(&current_id).unwrap()).into(),
                    (*node_ids.get(id).unwrap()).into(),
                    ()
                );
            }
        }
        ast::ExpressionKind::Unary(_, expr) =>
            find_usages_in_expression(expr, resolved, project_tree, graph, current_id, node_ids)?,
        ast::ExpressionKind::Paren(expr) => 
            find_usages_in_expression(expr, resolved, project_tree, graph, current_id, node_ids)?,
        ast::ExpressionKind::Field(expr, _) => 
            find_usages_in_expression(expr, resolved, project_tree, graph, current_id, node_ids)?,
        ast::ExpressionKind::Call { callee, generics: _, arguments } => {
            find_usages_in_expression(callee, resolved, project_tree, graph, current_id, node_ids)?;

            for arg in arguments.iter() {
                find_usages_in_expression(arg, resolved, project_tree, graph, current_id, node_ids)?;
            }
        }
        ast::ExpressionKind::Binary(a, _, b) => {
            find_usages_in_expression(a, resolved, project_tree, graph, current_id, node_ids)?;
            find_usages_in_expression(b, resolved, project_tree, graph, current_id, node_ids)?;
        }
        ast::ExpressionKind::AnonStruct(fields) | ast::ExpressionKind::StructLiteral(_, fields) => {
            for field in fields.iter() {
                find_usages_in_expression(&field.value, resolved, project_tree, graph, current_id, node_ids)?;
            }
        }
        ast::ExpressionKind::Assignment(a, b) => {
            find_usages_in_expression(a, resolved, project_tree, graph, current_id, node_ids)?;
            find_usages_in_expression(b, resolved, project_tree, graph, current_id, node_ids)?;
        },
        _ => {}
    }

    Ok(())
}

pub(crate) fn find_unused_nodes(
    graph: &DiGraph<NodeId, ()>,
    root_id: NodeId,
    node_ids: &HashMap<NodeId, NodeIndex>
) -> HashSet<NodeId> {
    let result = dijkstra(
        graph,
        (*node_ids.get(&root_id).unwrap()).into(),
        None,
        |_| 1
    );

    let mut unused = HashSet::new();
    for node_idx in graph.node_indices() {
        if !result.contains_key(&node_idx) {
            unused.insert(graph[node_idx]);
        }
    }
    unused
}
