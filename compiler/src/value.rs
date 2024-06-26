use ahash::HashMap;
use codegen::{blocks::{BlocksBuilder, Sb3Field, Sb3Value}, DataId};
use error::CompilerError;
use parser::ast::NodeId;
use resolution::resolve::ResolvedBlock;

use crate::CompileNotes;

#[derive(Debug, Clone)]
pub enum Value {
    Empty,
    Text(String),
    Number(f64),
    BlockRef(ResolvedBlock),
    FunctionRef(NodeId),
    BlockCall(DataId),
    Variable(DataId),
    Struct(HashMap<String, Value>)
}

impl Value {
    pub fn flatten(self) -> Vec<Value> {
        let mut value = Vec::new();
        match self {
            Value::Struct(fields) => {
                let mut keys: Vec<&String> = fields.keys().collect();
                keys.sort();
                for key in keys.iter() {
                    let field = fields.get(*key).unwrap();
                    value.push(field.clone());
                }
            }
            v @ _ => value.push(v)
        }
        value
    }

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
            Self::BlockCall(call) => {
                {
                    let mut block = builder.get_block_mut(&call);
                    block.parent = Some(possible_parent.clone());
                    block.next = None;
                }
                builder.set_previous(possible_parent.clone());
                // Update all that had this as their next.
                for block in builder.code_mut().blocks.values_mut() {
                    if block.next.as_ref() == Some(&call) {
                        block.next = Some(possible_parent.clone());
                    }
                }
                Ok(Sb3Value::Ptr(call))
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
}
