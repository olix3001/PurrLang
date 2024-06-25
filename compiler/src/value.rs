use codegen::{blocks::{BlocksBuilder, Sb3Value}, DataId};
use error::CompilerError;
use resolution::resolve::ResolvedBlock;

use crate::CompileNotes;

#[derive(Debug, Clone)]
pub enum Value {
    Text(String),
    Number(f64),
    BlockRef(ResolvedBlock),
    BlockCall(DataId)
}

impl Value {
    pub fn into_sb3(
        self,
        builder: &mut BlocksBuilder,
        possible_parent: &DataId
    ) -> Result<Sb3Value, CompilerError> {
        match self {
            Self::Text(text) =>
                Ok(Sb3Value::Text(text)),
            Self::Number(number) =>
                Ok(Sb3Value::Number(number)),
            _ => panic!("Temporary Error: Type {self:?} is not convertible to scratch")
        }
    }

    pub fn should_shadow(
        &self,
    ) -> bool {
        true
    }
}
