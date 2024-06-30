use std::{collections::HashMap, path::PathBuf};

use internment::Intern;

pub type FileRange = core::ops::Range<usize>;

#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub enum PurrSource {
    File(PathBuf),
    String(Intern<String>),
    #[default]
    Unknown
}

#[derive(Default, Debug, Clone)]
pub struct Libraries {
    pub libs: HashMap<String, PurrLib>
}

impl Libraries {
    pub fn register(&mut self, name: impl AsRef<str>, lib: PurrLib) {
        self.libs.insert(name.as_ref().to_string(), lib);
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&PurrLib> {
        self.libs.get(name.as_ref())
    }
}

#[derive(Debug, Clone)]
pub struct PurrLib {
    pub path: PathBuf
}

