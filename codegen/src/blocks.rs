use std::collections::HashMap;

use serde::{Serialize, ser::{SerializeSeq, SerializeTuple}};

use crate::DataId;

#[derive(Default, Debug, Clone, Serialize)]
pub struct Sb3Code {
    pub blocks: HashMap<DataId, Sb3Block>,
    pub variables: HashMap<DataId, Sb3Variable>
}

#[derive(Debug, Clone)]
pub struct Sb3Variable {
    pub name: String,
    pub value: usize
}

impl Serialize for Sb3Variable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer {
        let mut tuple = serializer.serialize_tuple(2)?;
        tuple.serialize_element(&self.name)?;
        tuple.serialize_element(&self.value)?;
        tuple.end()
    }
}

impl Sb3Variable {
    pub fn new(name: impl AsRef<str>) -> Self {
        Self {
            name: name.as_ref().to_string(),
            value: 0
        }
    }
}

#[derive(Default, Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Block {
    pub opcode: String,
    pub next: Option<DataId>,
    pub parent: Option<DataId>,
    pub inputs: HashMap<String, Sb3Input>,
    pub fields: HashMap<String, Sb3Input>,
    pub shadow: bool,
    pub top_level: bool,
    pub x: Option<i32>,
    pub y: Option<i32>
}

#[derive(Default, Debug, Clone)]
pub struct Sb3Input {
    pub kind: usize,
    pub values: Vec<Sb3Value>
}

impl Serialize for Sb3Input {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer {
        let mut seq = serializer.serialize_seq(Some(self.values.len() + 1))?;
        seq.serialize_element(&self.kind)?;
        for value in self.values.iter() {
            seq.serialize_element(value)?;
        }
        seq.end()
    }
}

#[derive(Debug, Clone)]
pub enum Sb3Value {
    Ptr(DataId),
    Text(String),
    Number(f64)
}

impl Serialize for Sb3Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer {
        match self {
            Self::Ptr(id) =>
                serializer.serialize_str(&id.0),
            Self::Text(text) => {
                let mut tuple = serializer.serialize_tuple(2)?;
                tuple.serialize_element(&10)?;
                tuple.serialize_element(text)?;
                tuple.end()
            },
            Self::Number(number) => {
                let mut tuple = serializer.serialize_tuple(2)?;
                tuple.serialize_element(&4)?;
                tuple.serialize_element(number)?;
                tuple.end()
            }
        }
    }
}

pub struct BlocksBuilder<'a> {
    code: &'a mut Sb3Code,
    start_x: i32,
    start_y: i32,
    previous: Option<DataId>
}

impl<'a> BlocksBuilder<'a> {
    pub fn new(code: &'a mut Sb3Code) -> Self {
        Self {
            code,
            start_x: 0,
            start_y: 0,
            previous: None
        }
    }

    pub fn finish(self) -> Option<DataId> {
        self.previous.clone()
    }

    pub fn block(
        &mut self,
        opcode: impl AsRef<str>,
    ) -> BlockBuilder {
        let id = DataId::new();
        let mut block = Sb3Block::default();
        block.opcode = opcode.as_ref().to_string();
        if let Some(parent) = &self.previous {
            block.parent = Some(parent.clone());

            if let Some(parent_block) = self.code.blocks.get_mut(parent) {
                parent_block.next = Some(id.clone());
            }
        } else {
            block.x = Some(self.start_x);
            block.y = Some(self.start_y);
        }

        self.code.blocks.insert(id.clone(), block);

        BlockBuilder::new(
            id.clone(),
            self.code.blocks.get_mut(&id).unwrap()
        )
    }
}

pub struct BlockBuilder<'a> {
    id: DataId,
    block: &'a mut Sb3Block
}

impl<'a> BlockBuilder<'a> {
    fn new(id: DataId, block: &'a mut Sb3Block) -> Self {
        Self { id, block }
    }

    pub fn finish(self) -> DataId { self.id }

    pub fn top_level(&mut self) -> &mut Self {
        self.block.top_level = true;
        self
    }

    pub fn shadow(&mut self) -> &mut Self {
        self.block.shadow = true;
        self
    }

    pub fn input(
        &mut self,
        name: impl AsRef<str>,
        is_shadow: bool,
        values: &[Sb3Value]
    ) -> &mut Self {
        self.block.inputs.insert(
            name.as_ref().to_string(),
            Sb3Input {
                kind: if is_shadow { 1 } else { 2 },
                values: values.to_vec()
            }
        );
        self
    }
}
