use std::{collections::{HashMap, hash_map::Entry}, fs};
use common::PurrSource;
use error::ariadne;

#[derive(Default)]
pub struct PurrCache {
    files: HashMap<PurrSource, ariadne::Source>
}

impl ariadne::Cache<PurrSource> for PurrCache {
    type Storage = String;

    fn fetch(&mut self, id: &PurrSource) -> Result<&ariadne::Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        Ok(match self.files.entry(id.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(ariadne::Source::from(
                match id {
                    PurrSource::Unknown => panic!("Error in unknown source"),
                    PurrSource::File(path) => fs::read_to_string(path).map_err(|e| Box::new(e) as _)?,
                    PurrSource::String(src) => src.as_ref().clone()
                }
            ))
        }) 
    }

    fn display<'a>(&self, id: &'a PurrSource) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match id {
            PurrSource::File(path) => Some(Box::new(path.display())),
            PurrSource::String(_) => Some(Box::new("source")),
            _ => None
        } 
    }
}
