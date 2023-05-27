use crate::*;

pub struct VecEncoder<'a, T, B: Backend> {
    size: IntegerId,
    inner: Encoder<'a, B>,
    written: usize,
    _phantom: PhantomData<*const T>,
}

impl<'a, T, B: Backend> VecEncoder<'a, T, B> {
    pub(super) fn new(mut encoder: Encoder<'a, B>) -> Self {
        Self {
            size: encoder.register_integer(),
            inner: encoder,
            written: 0,
            _phantom: PhantomData,
        }
    }

    pub fn push(&mut self, value: T)
    where
        T: Encode,
    {
        self.inner.encode(value);
        self.written += 1;
    }

    pub(super) fn inc_written(&mut self) {
        self.written += 1;
    }

    pub(super) fn write_len(&mut self) {
        self.inner.integers.update(self.size, self.written);
    }

    pub(super) fn flush(&mut self) {
        self.write_len();
        self.written = 0;
        self.size = self.inner.register_integer();
    }

    pub(super) fn finish(mut self) -> Encoder<'a, B> {
        self.write_len();
        take_field_and_leak(self, |s| ptr::addr_of!(s.inner))
    }

    pub(super) fn written(&self) -> usize {
        self.written
    }

    pub(super) fn inner(&mut self) -> Encoder<B> {
        self.inner.stack_borrow()
    }
}

impl<'a, T: Encode, B: Backend> Extend<T> for VecEncoder<'a, T, B> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for value in iter {
            self.push(value);
        }
    }
}

impl<'a, 'b, T: Encode, B: Backend> Extend<&'b T> for VecEncoder<'a, T, B> {
    fn extend<I: IntoIterator<Item = &'b T>>(&mut self, iter: I) {
        for value in iter {
            self.push(*value);
        }
    }
}

impl<T, B: Backend> Drop for VecEncoder<'_, T, B> {
    fn drop(&mut self) {
        self.write_len();
    }
}
