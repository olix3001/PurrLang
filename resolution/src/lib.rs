use ahash::{HashMap, HashMapExt};
use common::PurrSource;
use error::CompilerError;
use parser::ast;
use project_tree::{ProjectTree, ResolutionPath};
use resolve::{ResolutionNotes, ResolvedData};

pub mod project_tree;
pub mod resolve;

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTy {
    Never,
    Void,
    Ptr,

    Number,
    Text,
    Bool,

    Path(parser::ast::NodeId),
    Struct(HashMap<String, Self>),
    Function(Vec<Self>, Box<Self>),
    TyVar(String)
}

impl ResolvedTy {
    fn from_ast_ty(
        ty: &ast::Ty,
        current: &ResolutionPath,
        tree: &project_tree::ProjectTree,
        file: &PurrSource
    ) -> Result<Self, CompilerError> {
        match &ty.kind {
            ast::TyKind::Never => Ok(Self::Never),
            ast::TyKind::Infer => unreachable!("If you're reading this my code is broken :D (INFER TYPE)"),
            ast::TyKind::Void => Ok(Self::Void),
            ast::TyKind::Ptr => Ok(Self::Ptr),
            ast::TyKind::Number => Ok(Self::Number),
            ast::TyKind::Bool => Ok(Self::Bool),
            ast::TyKind::Text => Ok(Self::Text),
            ast::TyKind::Path(path) => {
                // Maybe we need to resolve first segment from import.
                let path_span = path.pos.clone();
                let path: ResolutionPath = path.clone().into();

                if let Some(target) = tree.resolve_lib(&path) {
                    return Ok(Self::Path(target));
                }

                if let Some(target) = tree.resolve_name(
                    &current.clone().join(&path)
                ) {
                    return Ok(Self::Path(target));
                }

                if let Some(current) = tree.resolve_tree(current) {
                    if path.segments.len() < 1 { return Err(CompilerError::UnknownPath {
                        path: path.segments,
                        pos: path_span,
                        file: file.clone()
                    }); }

                    if let Some(prefix) = current.imports.get(path.segments.first().unwrap()) {
                        let path = ResolutionPath {
                            segments: path.segments.iter().skip(1).cloned().collect()
                        };
                        let path = prefix.clone().join(&path);

                        if let Some(target) = tree.resolve_lib(&path) {
                            return Ok(Self::Path(target));
                        }
                        if let Some(target) = tree.resolve_name(&path) {
                            return Ok(Self::Path(target));
                        }
                    }
                }

                Err(CompilerError::UnknownPath {
                    path: path.segments,
                    pos: path_span,
                    file: file.clone()
                })
            },
            ast::TyKind::AnonStruct(fields) => {
                let mut fields_map = HashMap::new();

                for field in fields.iter() {
                    fields_map.insert(
                        field.name.clone(),
                        ResolvedTy::from_ast_ty(&field.ty, current, tree, file)?
                    );
                }

                Ok(Self::Struct(fields_map))
            },
            ast::TyKind::Function(args, ret) => {
                let mut args_ = Vec::new();

                for arg in args.iter() {
                    args_.push(ResolvedTy::from_ast_ty(arg, current, tree, file)?);
                }

                Ok(ResolvedTy::Function(
                    args_,
                    Box::new(ResolvedTy::from_ast_ty(&ret, current, tree, file)?)
                ))
            }
        }
    }

    pub fn pretty_name(&self, _tree: &ProjectTree) -> String {
        format!("{:?}", self) // Temporary.
    }

    pub fn matches(
        &self,
        target: &Self,
        resolved: &ResolvedData,
        notes: &ResolutionNotes
    ) -> bool {
        match (&self.resolve_to_top(resolved), &target.resolve_to_top(resolved)) {
            (ResolvedTy::Struct(fields_a), ResolvedTy::Struct(fields_b)) => {
                if fields_a.len() != fields_b.len() { return false; }
                for (name_a, value_a) in fields_a.iter() {
                    let Some(value_b) = fields_b.get(name_a) else { return false };
                    if !value_a.matches(value_b, resolved, notes) { return false }
                }
                true
            },
            (ResolvedTy::Number, ResolvedTy::Text) => true,
            (ResolvedTy::Bool, ResolvedTy::Text) => true,

            // Temporary solution
            (ResolvedTy::Text, ResolvedTy::Number) => true,

            (a, b) => a == b
        }
    }

    pub fn resolve_to_top(&self, resolved: &ResolvedData) -> ResolvedTy {
        match self {
            ResolvedTy::Path(path) => {
                resolved.types.get(path).unwrap().resolve_to_top(resolved)
            },
            other => other.clone()
        }
    }

    pub fn size(&self, resolved: &ResolvedData) -> usize {
        match self.resolve_to_top(resolved) {
            Self::Void => 0,
            Self::Text => 1,
            Self::Bool => 1,
            Self::Number => 1,
            Self::Never => 0,
            Self::Path(_) => unreachable!(),
            Self::TyVar(_) => unimplemented!(),
            Self::Function(_, _) => panic!("Function type is not sized"),
            Self::Struct(fields) => {
                let mut size = 0;
                for field in fields.iter() {
                    size += field.1.size(resolved);
                }
                size
            },
            Self::Ptr => panic!("Ptr cannot be used as value")
        }
    }

    pub fn flatten(&self, resolved: &ResolvedData) -> Vec<ResolvedTy> {
        let mut ty = Vec::new();
        match self.resolve_to_top(resolved) {
            s @ Self::Text | s @ Self::Bool | s @ Self::Number =>
                ty.push(s),
            Self::Struct(fields) => {
                let mut keys: Vec<&String> = fields.keys().collect();
                keys.sort();
                for key in keys.iter() {
                    let field = fields.get(*key).unwrap();
                    ty.push(field.clone());
                }
            },
            _ => {}
        }
        ty
    }
}
