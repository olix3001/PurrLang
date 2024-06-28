use std::{collections::HashMap, path::PathBuf};

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct Project {
    pub(crate) project: ProjectConfig,
    pub(crate) sprites: HashMap<String, Sprite>
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProjectConfig {
    pub(crate) extensions: Vec<String>
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sprite {
    pub(crate) costume: String,
    pub(crate) costumes: Vec<Costume>,
    #[serde(default)]
    pub(crate) position: Position,
    #[serde(default = "default_volume")]
    pub(crate) volume: u32,
    #[serde(default = "default_size")]
    pub(crate) size: u32,
    #[serde(default = "default_direction")]
    pub(crate) direction: i32,
    #[serde(default = "default_true")]
    pub(crate) visible: bool,
    #[serde(default = "default_rotation_style")]
    pub(crate) rotation_style: String,
    #[serde(default = "default_layer")]
    pub(crate) layer: u32,
    #[serde(default)]
    pub(crate) draggable: bool,
    pub(crate) entry: Option<PathBuf>
}

#[derive(Debug, Clone, Deserialize)]
pub struct Costume {
    pub(crate) name: String,
    pub(crate) src: PathBuf
}

#[derive(Default, Debug, Clone, Deserialize)]
pub struct Position {
    pub(crate) x: i32,
    pub(crate) y: i32
}

fn default_volume() -> u32 { 100 }
fn default_size() -> u32 { 100 }
fn default_direction() -> i32 { 90 }
fn default_true() -> bool { true }
fn default_rotation_style() -> String { "all around".to_string() }
fn default_layer() -> u32 { 1 }
