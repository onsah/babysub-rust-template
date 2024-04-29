use std::time::Instant;


pub struct Stats {
    pub checked: usize,
    pub parsed: usize,
    pub subsumed: usize,
    pub start_time: Instant,
}
