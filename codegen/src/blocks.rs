use std::{cell::{RefCell, RefMut}, collections::HashMap, rc::Rc};

use serde::{Serialize, ser::{SerializeSeq, SerializeTuple, SerializeStruct}};

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
    pub fields: HashMap<String, Sb3Field>,
    pub shadow: bool,
    pub top_level: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub x: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub y: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mutation: Option<Mutation>
}

#[derive(Debug, Clone)]
pub struct Mutation {
    proccode: String,
    argumentids: Vec<String>,
    argumentnames: Vec<String>,
    argumentdefaults: Vec<String>,
    warp: bool
}

impl Serialize for Mutation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer {
        let mut ser = serializer.serialize_struct("mutation", 7)?;
        ser.serialize_field("tagName", "mutation")?;
        ser.serialize_field::<[u8]>("children", &[])?;

        ser.serialize_field("proccode", &self.proccode)?;
        ser.serialize_field("argumentids", 
            &serde_json::ser::to_string(&self.argumentids).unwrap())?;
        ser.serialize_field("argumentnames", 
            &serde_json::ser::to_string(&self.argumentnames).unwrap())?;
        ser.serialize_field("argumentdefaults", 
            &serde_json::ser::to_string(&self.argumentdefaults).unwrap())?;
        ser.serialize_field("warp", if self.warp { "true" } else { "false" })?;
        ser.end()
    }
}

impl Default for Mutation {
    fn default() -> Self {
        Self {
            proccode: String::new(),
            argumentids: Vec::new(),
            argumentnames: Vec::new(),
            argumentdefaults: Vec::new(),
            warp: false
        }
    }
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
pub enum Sb3Field {
    Variable(DataId, String),
    Argument(String)
}

impl Serialize for Sb3Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer {
        match &self {
            Self::Variable(id, name) => {
                let mut tuple = serializer.serialize_tuple(2)?;
                tuple.serialize_element(&name)?;
                tuple.serialize_element(&id)?;
                tuple.end()
            }
            Self::Argument(name) => {
                let mut tuple = serializer.serialize_tuple(2)?;
                tuple.serialize_element(&name)?;
                tuple.serialize_element(&serde_json::Value::Null)?;
                tuple.end()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Sb3Value {
    Ptr(DataId),
    Text(String),
    Number(f64),
    Variable(DataId, String)
}

impl Sb3Value {
    pub fn is_shadow(&self) -> bool {
        match self {
            Self::Ptr(..) | Self::Variable(..) => false,
            _ => true
        }
    }
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
            },
            Self::Variable(variable, name) => {
                let mut tuple = serializer.serialize_tuple(3)?;
                tuple.serialize_element(&12)?;
                tuple.serialize_element(name)?;
                tuple.serialize_element(variable)?;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgumentTy {
    TextOrNumber,
    Boolean
}

#[derive(Default, Debug, Clone)]
pub struct Sb3FunctionDefinition {
    pub warp: bool,
    pub arguments: Vec<DataId>,
    pub proccode: String
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

    pub fn code_mut(
        &mut self
    ) -> std::cell::RefMut<'_, Sb3Code> {
        self.code.borrow_mut()
    }

    pub fn set_previous(
        &mut self,
        previous: DataId
    ) {
        self.data.borrow_mut().previous = Some(previous);
    }

    pub fn previous(&self) -> DataId {
        self.data.borrow().previous.as_ref().unwrap().clone()
    }

    pub fn get_block_mut(
        &mut self,
        id: &DataId
    ) -> std::cell::RefMut<'_, Sb3Block> {
        let borrowed = self.code.borrow_mut();
        RefMut::map(borrowed, |inner| inner.blocks.get_mut(id).unwrap())
    }

    pub fn define_variable(
        &mut self,
        display_name: impl AsRef<str>
    ) -> DataId {
        let mut code = self.code.borrow_mut();
        let id = DataId::new();
        code.variables.insert(
            id.clone(),
            Sb3Variable::new(display_name)
        );
        id
    }

    pub fn get_variable_name(
        &self,
        id: &DataId
    ) -> String {
        self.code.borrow().variables.get(id)
            .unwrap().name.clone()
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

    pub fn call_function(
        &mut self,
        definition: &Sb3FunctionDefinition,
        arguments: &[Sb3Value]
    ) {
        let mut call = self.block("procedures_call");
        for (arg_id, arg) in definition.arguments.iter().zip(arguments.iter()) {
            call.input(&arg_id.0, &[arg.clone()]);
        }

        {
            let call = call.block.as_mut().unwrap();
            call.mutation = Some(Mutation::default());
            let mutation = call.mutation.as_mut().unwrap();
            mutation.warp = definition.warp;
            mutation.proccode = definition.proccode.clone();
            mutation.argumentids = definition.arguments.iter()
                .map(|arg| arg.0.clone()).collect();
        }
    }

    pub fn define_function(
        &mut self,
        name: impl AsRef<str>,
        arguments: &[(String, ArgumentTy)],
        argument_ids: &[DataId],
        warp: bool
    ) -> (Self, Sb3FunctionDefinition) {
        let mut subbuilder = Self::new();
        subbuilder.code = self.code.clone();
        let mut definition = Sb3FunctionDefinition::default();
        definition.warp = warp;

        let mut proc_definition = subbuilder.block("procedures_definition");
        proc_definition.top_level();
        let mut proc_proto = subbuilder.block("procedures_prototype");
        proc_proto.shadow().parent(proc_definition.id.clone());

        {
            let proto = proc_proto.block.as_mut().unwrap();
            proto.mutation = Some(Mutation::default());
            let mutation = proto.mutation.as_mut().unwrap();
            mutation.warp = warp;
            mutation.proccode = format!(
                "{} {}", 
                name.as_ref(),
                arguments.iter().map(|arg|
                    if arg.1 == ArgumentTy::TextOrNumber { "%s" }
                    else { "%b" }
                ).collect::<Vec<_>>().join(" ")
            );
            definition.proccode = mutation.proccode.clone();
        }

        let mut arg_ids = Vec::new();
        for (arg, id) in arguments.iter().zip(argument_ids.iter()) {
            let id = id.clone();
            arg_ids.push(id.clone());

            let mut argdef = subbuilder.block(
                if arg.1 == ArgumentTy::TextOrNumber {
                    "argument_reporter_string_number"
                } else { "argument_reporter_boolean" }
            );
            argdef
                .parent(proc_proto.id().clone())
                .shadow()
                .field("VALUE", Sb3Field::Argument(arg.0.clone()));
            let argdef = argdef.finish();

            proc_proto.input(&id.0, &[
                Sb3Value::Ptr(argdef)
            ]);

            let mutation = proc_proto.block.as_mut().unwrap()
                .mutation.as_mut().unwrap();
            mutation.argumentids.push(id.0.clone());
            mutation.argumentnames.push(arg.0.clone());
            mutation.argumentdefaults.push(
                if arg.1 == ArgumentTy::TextOrNumber { "" }
                else { "false" }.to_string()
            );

            definition.arguments.push(id);
        }

        proc_definition.input("custom_block", &[
            Sb3Value::Ptr(proc_proto.finish())
        ]);
        let id = proc_definition.finish();

        subbuilder.data.borrow_mut().previous = Some(id);

        (subbuilder, definition)
    }

    pub fn subbuilder_for(
        &mut self,
        block: &DataId
    ) -> Self {
        let mut subbuilder = Self::new();
        subbuilder.code = self.code.clone();
        subbuilder.data.borrow_mut().previous = Some(block.clone());
        subbuilder
    }
}

pub struct BlockBuilder {
    id: DataId,
    block: Option<Sb3Block>,
    builder: BlocksBuilder,
    skip_set_previous: bool,
}

impl BlockBuilder {
    fn new(id: DataId, block: Sb3Block, builder: BlocksBuilder) -> Self {
        Self { id, block: Some(block), builder, skip_set_previous: false }
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

    pub fn parent(&mut self, id: DataId) -> &mut Self {
        let block = self.block.as_mut().unwrap();
        block.parent = Some(id.clone());
        block.x = None;
        block.y = None;
        block.next = None;
        for block in self.builder.code_mut().blocks.values_mut() {
            if block.next.as_ref() == Some(&self.id) {
                block.next = Some(id.clone());
            }
        }
        self.builder.data.borrow_mut().previous = Some(id);
        self.skip_set_previous = true;

        self
    }

    pub fn input(
        &mut self,
        name: impl AsRef<str>,
        values: &[Sb3Value]
    ) -> &mut Self {
        self.block.as_mut().unwrap().inputs.insert(
            name.as_ref().to_string(),
            Sb3Input {
                kind: if values[0].is_shadow() { 1 } else { 2 },
                values: values.to_vec()
            }
        );
        self
    }

    pub fn field(
        &mut self,
        name: impl AsRef<str>,
        field: Sb3Field
    ) -> &mut Self {
        self.block.as_mut().unwrap().fields.insert(
            name.as_ref().to_string(),
            field
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
        if !self.skip_set_previous {
            self.builder.data.borrow_mut()
                .previous = Some(self.id.clone());
        }
    }
}
