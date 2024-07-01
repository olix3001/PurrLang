use std::rc::Rc;

use ahash::{HashMap};

use common::Libraries;
use parser::ast::{self};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ResolutionPath {
    pub segments: Vec<String>,
}

impl From<ast::PurrPath> for ResolutionPath {
    fn from(value: ast::PurrPath) -> Self {
        Self {
            segments: value.segments.iter().map(|seg|
                seg.ident.clone()
            ).collect()
        }
    }
}

impl From<&[&str]> for ResolutionPath {
    fn from(value: &[&str]) -> Self {
        Self {
            segments: value.iter().map(|v| v.to_string()).collect()
        }
    }
}

impl ResolutionPath {
    pub fn join(mut self, other: &Self) -> Self {
        self.segments.extend_from_slice(&other.segments);
        self
    }
}

/// Project tree contains all information
/// about modules and data related to them.
#[derive(Default, Debug, Clone)]
pub struct ProjectTree {
    /// Modules can contain subtrees and this
    /// field maps those modules to their subtrees.
    pub subtrees: HashMap<String, ProjectTree>,
    pub imports: HashMap<String, ResolutionPath>,
    pub names: HashMap<String, ast::NodeId>,
    pub libs: Rc<Libraries>
}

impl ProjectTree {
    pub fn build_from_ast(
        prefix: ResolutionPath, 
        ast: &Vec<ast::Item>,
        libs: Rc<Libraries>
    ) -> Self {
        let mut tree = Self::default();
        for item in ast.iter() {
            match &item.kind {
                ast::ItemKind::Module(module) => {
                    tree.subtrees.insert(
                        module.name.clone(),
                        Self::build_from_ast(
                            prefix
                                .clone()
                                .join(&[module.name.as_str()].as_slice().into()),
                            &module.body,
                            Rc::clone(&libs)
                        )
                    );
                    tree.names.insert(
                        module.name.clone(),
                        item.id
                    );
                },
                ast::ItemKind::Import(import) => {
                    let fs = import.prefix.segments.first().unwrap();
                    if libs.libs.contains_key(&fs.ident) {
                        tree.resolve_import_tree(&Default::default(), import)
                    } else {
                        tree.resolve_import_tree(&prefix, import)
                    }
                }

                ast::ItemKind::BlockDefinition(block) => {
                    tree.names.insert(
                        block.name.clone(),
                        item.id
                    );
                },
                ast::ItemKind::StructDefinition(struct_) => {
                    tree.names.insert(
                        struct_.name.clone(),
                        item.id
                    );
                },
                ast::ItemKind::FunctionDefinition(function) => {
                    tree.names.insert(
                        function.name.clone(),
                        item.id
                    );
                }

                _ => {}
            }
        }

        tree.libs = libs;
        tree
    }

    fn resolve_import_tree(&mut self, prefix: &ResolutionPath, tree: &ast::ImportTree) {
        let prefix: ResolutionPath = prefix.clone().join(&tree.prefix.clone().into());

        match &tree.kind {
            ast::ImportKind::Name => {
                let import_name = prefix.segments.last().cloned().unwrap();
                self.imports.insert(
                    import_name,
                    prefix
                );
            },
            ast::ImportKind::Glob => 
                // Glob imports are hard because we need to know all other
                // modules in oreder to resolve them.
                unimplemented!("Glob imports are not implemented in current version."),
            ast::ImportKind::Nested(subtrees) => {
                for subtree in subtrees.iter() {
                    self.resolve_import_tree(&prefix, subtree);
                }
            }
        }
    }

    pub fn resolve_lib(&self, path: &ResolutionPath) -> Option<ast::NodeId> {
        if self.libs.get(path.segments.first().unwrap()).is_some() {
            self.resolve_name(path)
        } else { None }
    }

    pub fn resolve_name(&self, path: &ResolutionPath) -> Option<ast::NodeId> {
        let mut path = path.segments.iter();

        let mut current_tree = self;

        while path.len() > 1 {
            let segment = path.next().unwrap();
            current_tree = current_tree.subtrees.get(segment)?;
        }

        if let Some(segment) = path.next() {
            current_tree.names.get(segment).cloned()
        } else { None }
    }

    pub fn resolve_tree(&self, path: &ResolutionPath) -> Option<&ProjectTree> {
        let mut path = path.segments.iter();

        let mut current_tree = self;

        while path.len() > 1 {
            let segment = path.next().unwrap();
            current_tree = current_tree.subtrees.get(segment)?;
        }

        if let Some(segment) = path.next() {
            current_tree.subtrees.get(segment)
        } else { Some(current_tree) }
    }
}
