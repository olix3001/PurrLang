use ahash::HashMap;
use codegen::{blocks::{BlocksBuilder, Sb3Field, Sb3Value}, DataId};
use error::CompilerError;
use resolution::resolve::ResolvedBlock;

use crate::CompileNotes;

#[derive(Debug, Clone)]
pub enum Value {
    Empty,
    Text(String),
    Number(f64),
    BlockRef(ResolvedBlock),
    BlockCall(DataId),
    Variable(DataId),
    Struct(HashMap<String, Value>)
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
            Self::Variable(id) => {
                let name = builder.get_variable_name(&id);
                Ok(Sb3Value::Variable(id, name))
            },
            _ => panic!("Temporary Error: Type {self:?} is not convertible to scratch input.")
        }
    }

    pub fn as_sb3_field(
        &self,
        builder: &mut BlocksBuilder,
    ) -> Result<Sb3Field, CompilerError> {
        match self {
            Self::Variable(id) => {
                let name = builder.get_variable_name(&id);
                Ok(Sb3Field::Variable(id.clone(), name))
            },
            _ => panic!("Temporary Error: Type {self:?} is not convertible to scratch field.")
        }
    }

    pub fn should_shadow(
        &self,
    ) -> bool {
        true
    }
}
