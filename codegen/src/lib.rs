use std::sync::atomic::AtomicU64;

pub mod blocks;

static DATA_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize)]
pub struct DataId(String);

impl DataId {
    pub fn new() -> Self {
        let id = DATA_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Self(format!("{id:08x}"))
    }
}
