use ahash::{HashMap, HashMapExt};
use common::PurrSource;
use error::CompilerError;
use parser::ast;
use project_tree::ResolutionPath;

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
            ast::TyKind::Text => Ok(Self::Text),
            ast::TyKind::Path(path) => {
                // Maybe we need to resolve first segment from import.
                let path_span = path.pos.clone();
                let path: ResolutionPath = path.clone().into();
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

    pub fn pretty_name(&self) -> String {
        format!("{:?}", self) // Temporary.
    }
}
