const SIZE: usize = 1024;

/// This keeps around at most SIZE * 2 values without shifting values
/// around.
#[derive(Clone)]
pub struct History<T> {
    /// Current history.
    a: Vec<T>,
    /// Backup history.
    b: Vec<T>,
}

impl<T> History<T> {
    pub fn new() -> Self {
        Self::with_capacity(SIZE)
    }

    pub fn with_capacity(size: usize) -> Self {
        Self {
            a: Vec::with_capacity(size),
            b: Vec::with_capacity(size),
        }
    }

    pub fn push(&mut self, value: T) {
        // If a is full, swap a and b and clear a. This drops the
        // backup history.
        if self.a.len() == self.a.capacity() {
            std::mem::swap(&mut self.a, &mut self.b);
            self.a.clear();
        }

        self.a.push(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.a.is_empty() {
            self.b.pop()
        } else {
            self.a.pop()
        }
    }
}
