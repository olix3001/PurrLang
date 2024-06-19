use std::path::PathBuf;

use internment::Intern;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PurrSource {
    File(PathBuf),
    String(Intern<String>)
}
