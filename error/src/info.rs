use common::{FileRange, PurrSource};

#[derive(Debug, Default)]
pub struct ErrorInfo {
    pub position: CodeArea
}

impl ErrorInfo {
    pub fn from_area(area: CodeArea) -> Self {
        Self {
            position: area,
            ..Default::default() // Future proofing
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeArea {
    pub file: common::PurrSource,
    pub pos: FileRange
}

impl Default for CodeArea {
    fn default() -> Self {
        Self {
            file: PurrSource::Unknown,
            pos: 0..0
        }
    }
}

impl ariadne::Span for CodeArea {
    type SourceId = PurrSource;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize { self.pos.start }
    fn end(&self) -> usize { self.pos.end }
}
