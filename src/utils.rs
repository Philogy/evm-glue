#[derive(Debug, Default)]
pub struct MarkTracker(usize);

impl MarkTracker {
    pub fn next_mark(&mut self) -> usize {
        let new_mid = self.0;
        self.0 = new_mid + 1;
        new_mid
    }
}
