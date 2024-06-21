use std::path::PathBuf;

use internment::Intern;

pub type FileRange = core::ops::Range<usize>;

#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub enum PurrSource {
    File(PathBuf),
    String(Intern<String>),
    #[default]
    Unknown
}
