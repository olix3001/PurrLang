use std::collections::{HashMap, HashSet};

use error::CompilerError;
use parser::ast::{self, NodeId};
use resolution::{project_tree::ProjectTree, resolve::ResolvedData};
use tree_shaking::build_usage_graph;

use crate::tree_shaking::find_unused_nodes;

mod tree_shaking;

pub fn optimize(
    ast: &mut Vec<ast::Item>,
    resolved: &ResolvedData,
    project_tree: &ProjectTree
) -> Result<HashSet<NodeId>, CompilerError> {
    let mut node_ids = HashMap::new();
    let (root_id, usage_graph) = build_usage_graph(
        ast,
        resolved, 
        &mut node_ids,
        project_tree
    )?;
    let unused_nodes = find_unused_nodes(
        &usage_graph,
        root_id,
        &node_ids
    );

    // TODO: Inline variables used only once.

    Ok(unused_nodes)
}
