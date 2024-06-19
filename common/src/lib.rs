use std::path::PathBuf;

use internment::Intern;

pub type FileRange = core::ops::Range<usize>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PurrSource {
    File(PathBuf),
    String(Intern<String>),
    Unknown
}
