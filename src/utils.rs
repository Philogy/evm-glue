macro_rules! debug_as_display {
    ($obj:tt) => {
        impl fmt::Display for $obj {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    };
}

#[derive(Default)]
pub struct MarkTracker(usize);

impl MarkTracker {
    pub fn next_mark(&mut self) -> usize {
        let new_mid = self.0;
        self.0 = new_mid + 1;
        new_mid
    }
}

pub(crate) use debug_as_display;
