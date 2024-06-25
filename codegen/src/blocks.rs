use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub x: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
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

#[derive(Default)]
struct InnerBuilderData {
    previous: Option<DataId>
}

#[derive(Clone)]
pub struct BlocksBuilder {
    code: Rc<RefCell<Sb3Code>>,
    start_x: i32,
    start_y: i32,
    data: Rc<RefCell<InnerBuilderData>>
}

impl BlocksBuilder {
    pub fn new() -> Self {
        Self {
            code: Rc::new(RefCell::new(
                Sb3Code::default()
            )),
            start_x: 0,
            start_y: 0,
            data: Rc::new(RefCell::new(
                InnerBuilderData::default()
            ))
        }
    }

    pub fn finish(self) -> Option<Sb3Code> {
        Some(self.code.take()) // TODO: Add parent
    }

    pub fn block(
        &mut self,
        opcode: impl AsRef<str>,
    ) -> BlockBuilder {
        let id = DataId::new();
        let mut block = Sb3Block::default();
        let mut code = self.code.borrow_mut();
        block.opcode = opcode.as_ref().to_string();
        if let Some(parent) = &self.data.borrow().previous {
            block.parent = Some(parent.clone());

            if let Some(parent_block) = code.blocks.get_mut(parent) {
                parent_block.next = Some(id.clone());
            }
        } else {
            block.x = Some(self.start_x);
            block.y = Some(self.start_y);
        }

        BlockBuilder::new(
            id.clone(),
            block,
            self.clone()
        )
    }
}

pub struct BlockBuilder {
    id: DataId,
    block: Option<Sb3Block>,
    builder: BlocksBuilder
}

impl BlockBuilder {
    fn new(id: DataId, block: Sb3Block, builder: BlocksBuilder) -> Self {
        Self { id, block: Some(block), builder }
    }

    pub fn finish(self) -> DataId { self.id.clone() }

    pub fn id(&self) -> &DataId { &self.id }

    pub fn top_level(&mut self) -> &mut Self {
        self.block.as_mut().unwrap().top_level = true;
        self
    }

    pub fn shadow(&mut self) -> &mut Self {
        self.block.as_mut().unwrap().shadow = true;
        self
    }

    pub fn input(
        &mut self,
        name: impl AsRef<str>,
        is_shadow: bool,
        values: &[Sb3Value]
    ) -> &mut Self {
        self.block.as_mut().unwrap().inputs.insert(
            name.as_ref().to_string(),
            Sb3Input {
                kind: if is_shadow { 1 } else { 2 },
                values: values.to_vec()
            }
        );
        self
    }
}

impl Drop for BlockBuilder {
    fn drop(&mut self) {
        self.builder.code.borrow_mut()
            .blocks.insert(
                self.id.clone(),
                self.block.take().unwrap()
            );
        self.builder.data.borrow_mut()
            .previous = Some(self.id.clone());
    }
}
