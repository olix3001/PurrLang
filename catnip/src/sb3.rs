use std::collections::HashMap;

use serde::Serialize;

#[derive(Default, Debug, Clone, Serialize)]
pub struct ScratchProject {
    pub(crate) targets: Vec<Target>,
    pub(crate) monitors: Vec<()>, // Unimplemented currently
    pub(crate) extensions: Vec<String>,
    pub(crate) meta: Meta
}

#[derive(Default, Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Target {
    pub(crate) is_stage: bool,
    pub(crate) name: String,
    pub(crate) current_costume: u32,
    pub(crate) costumes: Vec<ScratchCostume>,
    pub(crate) sounds: Vec<ScratchSound>,
    pub(crate) volume: u32,
    pub(crate) layer_order: u32,
    pub(crate) visible: bool,
    pub(crate) x: i32,
    pub(crate) y: i32,
    pub(crate) size: u32,
    pub(crate) draggable: bool,
    pub(crate) direction: i32,
    pub(crate) rotation_style: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) tempo: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) video_transparency: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) video_state: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) text_to_speech_language: Option<serde_json::Value>,

    // From codegen
    pub(crate) blocks: HashMap<codegen::DataId, codegen::blocks::Sb3Block>,
    pub(crate) variables: HashMap<codegen::DataId, codegen::blocks::Sb3Variable>,
    // Future
    pub(crate) lists: HashMap<codegen::DataId, ()>,
    pub(crate) broadcasts: HashMap<codegen::DataId, ()>,
    pub(crate) comments: HashMap<codegen::DataId, ()>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ScratchCostume {
    pub(crate) name: String,
    pub(crate) bitmap_resolution: u32,
    pub(crate) data_format: String,
    pub(crate) asset_id: String,
    pub(crate) md5ext: String,
    pub(crate) rotation_center_x: u32,
    pub(crate) rotation_center_y: u32
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ScratchSound {
    // TODO
}

#[derive(Debug, Clone, Serialize)]
pub struct Meta {
    pub(crate) semver: &'static str,
    pub(crate) vm: &'static str,
    pub(crate) agent: &'static str
}

impl Default for Meta {
    fn default() -> Self {
        Self {
            semver: "3.0.0",
            vm: "2.3.4",
            agent: "Mozilla/5.0"
        }
    }
}
