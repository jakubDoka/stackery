use core::{
    alloc::Allocator,
    cell::UnsafeCell,
    intrinsics::unlikely,
    iter, mem,
    ops::{Deref, Range},
    ptr::{self, NonNull},
    sync::atomic::{AtomicPtr, AtomicUsize},
};

use alloc::{alloc::Global, boxed::Box, slice, sync::Arc};

use self::refs::{ParRef, ParSlice};

pub mod refs;

pub unsafe auto trait NoInteriorMut {}

impl<T: ?Sized> !NoInteriorMut for UnsafeCell<T> {}

pub struct ParallelBase<T: NoInteriorMut, A: Allocator = Global> {
    lanes: Arc<[ParallelLane<T, A>]>,
    lens: Box<[usize]>,
}

unsafe impl<T: NoInteriorMut, A: Allocator + Send + Sync> Send for ParallelBase<T, A> {}
unsafe impl<T: NoInteriorMut, A: Allocator + Send + Sync> Sync for ParallelBase<T, A> {}

impl<T: NoInteriorMut> ParallelBase<T> {
    pub fn new(lanes: usize) -> Self {
        Self::new_in(Global, lanes)
    }
}

impl<T: NoInteriorMut, A: Allocator> ParallelBase<T, A> {
    pub fn new_in(alloc: A, lane_count: usize) -> Self
    where
        A: Clone,
    {
        let lanes =
            Arc::from_iter((0..lane_count).map(|_| ParallelLane::new(alloc.clone(), lane_count)));
        let lens = Box::from_iter((0..lanes.len()).map(|_| 0));
        Self { lanes, lens }
    }

    pub fn split(&self) -> impl Iterator<Item = Parallel<T, A>> + '_ {
        let lanes = self.lanes.clone();
        assert_eq!(
            Arc::strong_count(&lanes),
            2,
            "multiple instances of Parallel to view same lane is disallowed"
        );
        self.lens.iter().enumerate().map(|(id, &len)| Parallel {
            base: Self {
                lanes: self.lanes.clone(),
                lens: self.lens.clone(),
            },
            current_lane: id,
            current_len: len,
        })
    }

    pub fn pull_from(&mut self, thread: &Parallel<T, A>) {
        self.lens[thread.current_lane] = thread.current_len;
    }

    pub fn push_to(&self, thread: &mut Parallel<T, A>) {
        thread.base.lens.copy_from_slice(&self.lens);
    }
}

impl<T: NoInteriorMut, A: Allocator> Drop for ParallelBase<T, A> {
    fn drop(&mut self) {
        if let Some(lanes) = Arc::get_mut(&mut self.lanes) {
            for (lane, &len) in lanes.iter_mut().zip(self.lens.iter()) {
                unsafe {
                    let data = lane.get_local_data_slice().as_mut_ptr();
                    ptr::slice_from_raw_parts_mut(data, len).drop_in_place();
                }
            }
        }
    }
}

impl<T, A> serde_base128::Serde128 for ParallelBase<T, A>
where
    T: NoInteriorMut + serde_base128::Serde128,
    A: Allocator + Default,
{
    fn serialize(&self, encoder: &mut serde_base128::Encoder) {
        self.lens.serialize(encoder);
        for (lane, len) in self.lanes.iter().zip(self.lens.iter()) {
            unsafe {
                let data = lane.get_local_data_slice().as_mut_ptr();
                let slice = slice::from_raw_parts(data, *len);
                encoder.write_object_slice(slice);
            }
        }
    }

    unsafe fn deserialize(decoder: &mut serde_base128::Decoder) -> Self {
        let lens = Box::<[usize]>::deserialize(decoder);
        let lanes = lens
            .iter()
            .map(|&len| {
                let lane = ParallelLane::<T, _>::new(A::default(), lens.len());
                lane.grow(len.next_power_of_two());
                let data = lane.get_local_data_slice().as_mut_ptr();
                for i in 0..len {
                    unsafe {
                        let ptr = data.add(i);
                        ptr::write(ptr, T::deserialize(decoder));
                    }
                }
                lane
            })
            .collect::<Arc<[_]>>();
        Self { lanes, lens }
    }
}

pub struct Parallel<T: NoInteriorMut, A: Allocator = Global> {
    base: ParallelBase<T, A>,
    current_lane: usize,
    current_len: usize,
}

unsafe impl<T: NoInteriorMut, A: Allocator + Send + Sync> Send for Parallel<T, A> {}
unsafe impl<T: NoInteriorMut, A: Allocator + Send + Sync> Sync for Parallel<T, A> {}

impl<T: NoInteriorMut, A: Allocator> Parallel<T, A> {
    fn push(&mut self, value: T) -> ParRef<T> {
        let (lane, range) = self.push_low(value);
        ParRef::new(lane, range)
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = T>) -> ParSlice<T> {
        let (lane, range) = self.extend_low(iter);
        ParSlice::new(lane, range)
    }

    fn push_low(&mut self, value: T) -> (usize, usize) {
        let (lane, range) = self.extend_low(iter::once(value));
        (lane, range.start)
    }

    fn get(&self, pr: ParRef<T>) -> impl Deref<Target = T> + '_ {
        self.get_low(pr.lane(), pr.index())
    }

    fn get_mut(&mut self, pr: ParRef<T>) -> &mut T {
        assert_eq!(
            self.current_lane,
            pr.lane(),
            "cannot mutate from different lane"
        );
        self.get_mut_low(pr.index())
    }

    fn get_slice(&self, ps: ParSlice<T>) -> impl Deref<Target = [T]> + '_ {
        self.get_slice_low(ps.lane(), ps.range())
    }

    fn get_mut_slice(&mut self, ps: ParSlice<T>) -> &mut [T] {
        assert_eq!(
            self.current_lane,
            ps.lane(),
            "cannot mutate from different lane"
        );
        self.get_mut_slice_low(ps.range())
    }

    fn extend_low(&mut self, iter: impl IntoIterator<Item = T>) -> (usize, Range<usize>) {
        let lane = &self.base.lanes[self.current_lane];

        let mut slice = unsafe { lane.get_local_data_slice() };
        let mut cursor = self.current_len;

        for item in iter {
            if unlikely(cursor == slice.len()) {
                unsafe { lane.grow(ParallelLane::<T, A>::MIN_CAPACITY) };
                slice = unsafe { lane.get_local_data_slice() };
            }

            unsafe {
                let write_cursor = slice.as_mut_ptr().add(cursor);
                ptr::write(write_cursor, item);
            }

            cursor += 1;
        }

        (
            self.current_lane,
            mem::replace(&mut self.current_len, cursor)..cursor,
        )
    }

    fn get_low(&self, thread: usize, index: usize) -> impl Deref<Target = T> + '_ {
        let lane = &self.base.lanes[thread];
        let len = if thread == self.current_lane {
            self.current_len
        } else {
            self.base.lens[thread]
        };
        assert!(
            index < len,
            "index out of bounds: the len is {} but the index is {}",
            len,
            index
        );
        let (lane_lock, slice) = unsafe { lane.get_data_slice(self.current_lane) };

        struct Guard<'a, T: NoInteriorMut, L> {
            value: &'a T,
            _lane_lock: L,
        }

        impl<'a, T: NoInteriorMut, D> Deref for Guard<'a, T, D> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                self.value
            }
        }

        Guard {
            _lane_lock: lane_lock,
            value: unsafe { &*slice.as_mut_ptr().add(index) },
        }
    }

    fn get_mut_low(&mut self, index: usize) -> &mut T {
        let lane = &self.base.lanes[self.current_lane];
        let frozen_len = self.base.lens[self.current_lane];
        let len = self.current_len;

        assert!(
            index < len,
            "index out of bounds: the len is {} but the index is {}",
            len,
            index
        );
        assert!(
            index < frozen_len,
            "index out of bounds: the len is {} but the index is {}",
            frozen_len,
            index
        );

        let slice = unsafe { lane.get_local_data_slice().as_mut_ptr() };

        unsafe { &mut *slice.add(index) }
    }

    fn get_slice_low(&self, thread: usize, range: Range<usize>) -> impl Deref<Target = [T]> + '_ {
        let lane = &self.base.lanes[thread];
        let len = if thread == self.current_lane {
            self.current_len
        } else {
            self.base.lens[thread]
        };

        assert!(
            range.start <= range.end,
            "range start must be less than or equal to range end"
        );
        assert!(
            range.end <= len,
            "range end out of bounds: the len is {} but the range end is {}",
            len,
            range.end
        );
        let (lane_lock, slice) = unsafe { lane.get_data_slice(self.current_lane) };

        struct Guard<'a, T: NoInteriorMut, D> {
            _lane_lock: D,
            value: &'a [T],
        }

        impl<'a, T: NoInteriorMut, D> Deref for Guard<'a, T, D> {
            type Target = [T];

            fn deref(&self) -> &Self::Target {
                self.value
            }
        }

        Guard {
            _lane_lock: lane_lock,
            value: unsafe {
                slice::from_raw_parts(slice.as_mut_ptr().add(range.start), range.len())
            },
        }
    }

    fn get_mut_slice_low(&mut self, range: Range<usize>) -> &mut [T] {
        let lane = &self.base.lanes[self.current_lane];
        let frozen_len = self.base.lens[self.current_lane];
        let len = self.current_len;

        assert!(
            range.start <= range.end,
            "range start must be less than or equal to range end"
        );
        assert!(
            range.end <= len,
            "range end out of bounds: the len is {} but the range end is {}",
            len,
            range.end
        );
        assert!(
            range.start <= frozen_len,
            "range end out of bounds: the len is {} but the range end is {}",
            frozen_len,
            range.end
        );
        let slice = unsafe { lane.get_local_data_slice().as_mut_ptr() };

        unsafe { slice::from_raw_parts_mut(slice.add(range.start), range.len()) }
    }
}

impl<T: NoInteriorMut, A: Allocator> Drop for Parallel<T, A> {
    fn drop(&mut self) {
        let current_lane = &self.base.lanes[self.current_lane];
        let current_len = self.current_len;
        let visible_len = self.base.lens[self.current_lane];

        unsafe {
            let data = current_lane.get_local_data_slice().as_mut_ptr();
            ptr::slice_from_raw_parts_mut(data.add(visible_len), current_len - visible_len)
                .drop_in_place();
        }
    }
}

struct ParallelLane<T: NoInteriorMut, A: Allocator> {
    data: AtomicPtr<T>,
    cap: AtomicUsize,
    view_lock: ViewLock<A>,
}

impl<T: NoInteriorMut, A: Allocator> ParallelLane<T, A> {
    const GROW_FACTOR: usize = 2;
    const MIN_CAPACITY: usize = 8;

    fn new(alloc: A, lane_count: usize) -> Self {
        Self {
            data: AtomicPtr::new(core::ptr::null_mut()),
            cap: AtomicUsize::new(0),
            view_lock: ViewLock::new(lane_count, alloc),
        }
    }

    /// Safety: can only be called from controller thread
    unsafe fn grow(&self, min_size: usize) {
        let current_cap = self.cap.load(core::sync::atomic::Ordering::Relaxed);
        let new_cap = min_size.max(current_cap * Self::GROW_FACTOR);

        let current_ptr = self.data.load(core::sync::atomic::Ordering::Relaxed);
        let current_layout = core::alloc::Layout::array::<T>(current_cap).unwrap();

        let new_layout = core::alloc::Layout::array::<T>(new_cap).unwrap();

        let new_ptr = self
            .view_lock
            .alloc
            .allocate(new_layout)
            .unwrap()
            .as_mut_ptr();
        let new_ptr = new_ptr.cast::<T>();

        if !current_ptr.is_null() {
            core::ptr::copy_nonoverlapping(current_ptr, new_ptr, current_cap);
        }

        self.data
            .store(new_ptr, core::sync::atomic::Ordering::Relaxed);

        if let Some(ptr) = NonNull::new(current_ptr) {
            self.view_lock.switch_and_wait();
            self.view_lock.alloc.deallocate(ptr.cast(), current_layout);
        }

        self.cap
            .store(new_cap, core::sync::atomic::Ordering::Relaxed);
    }

    unsafe fn get_data_slice(&self, lane: usize) -> (impl Drop, *mut [T]) {
        (self.view_lock.lock(lane), self.get_local_data_slice())
    }

    unsafe fn get_local_data_slice(&self) -> *mut [T] {
        let ptr = self.data.load(core::sync::atomic::Ordering::Relaxed);
        let cap = self.cap.load(core::sync::atomic::Ordering::Relaxed);
        ptr::slice_from_raw_parts_mut(ptr, cap)
    }
}

impl<R: NoInteriorMut, A: Allocator> Drop for ParallelLane<R, A> {
    fn drop(&mut self) {
        let ptr = self.data.load(core::sync::atomic::Ordering::Relaxed);
        let cap = self.cap.load(core::sync::atomic::Ordering::Relaxed);
        let layout = core::alloc::Layout::array::<R>(cap).unwrap();
        unsafe {
            if let Some(ptr) = NonNull::new(ptr) {
                self.view_lock.alloc.deallocate(ptr.cast(), layout);
            }
        }
    }
}

struct ViewLockLane {
    counter: AtomicUsize,
    spacing: [usize; 7],
    // see the benchmark below, spacing makes threads fight less when acessing
    // guards
}

impl ViewLockLane {
    fn lock(&self) {
        self.counter.store(1, core::sync::atomic::Ordering::Release);
    }

    fn unlock(&self) {
        self.counter.store(0, core::sync::atomic::Ordering::Release);
    }

    fn get_state(&self) -> usize {
        self.counter.load(core::sync::atomic::Ordering::Acquire)
    }
}

struct ViewLock<A: Allocator> {
    lanes: AtomicPtr<ViewLockLane>,
    width: usize,
    switched: UnsafeCell<bool>,
    alloc: A,
}

impl<A: Allocator> ViewLock<A> {
    fn new(width: usize, alloc: A) -> Self {
        if width == 0 {
            return Self {
                lanes: AtomicPtr::new(core::ptr::null_mut()),
                width,
                switched: UnsafeCell::new(false),
                alloc,
            };
        }

        let layout = core::alloc::Layout::array::<ViewLockLane>(width * 2).unwrap();
        let ptr = alloc.allocate_zeroed(layout).unwrap().as_mut_ptr();

        Self {
            lanes: AtomicPtr::new(ptr.cast()),
            width,
            switched: UnsafeCell::new(false),
            alloc,
        }
    }

    /// Safety: can only be called from controller thread
    unsafe fn switch(&self) -> *mut ViewLockLane {
        let switched = self.switched.get();
        *switched = !*switched;
        let ptr = self.lanes.load(core::sync::atomic::Ordering::SeqCst);
        let new_ptr = if *switched {
            ptr.add(self.width)
        } else {
            ptr.sub(self.width)
        };
        self.lanes
            .store(new_ptr, core::sync::atomic::Ordering::SeqCst);
        ptr
    }

    /// Safety: can only be called from controller thread
    unsafe fn switch_and_wait(&self) {
        let ptr = self.switch();
        let slice = slice::from_raw_parts(ptr, self.width);
        while slice.iter().map(|l| l.get_state()).sum::<usize>() != 0 {}
    }

    /// Safety: the lane must be less then width
    unsafe fn lock(&self, lane: usize) -> impl Drop {
        let ptr = self.lanes.load(core::sync::atomic::Ordering::SeqCst);
        let lane = ptr.add(lane);
        (*lane).lock();
        struct Unlock(*mut ViewLockLane);
        impl Drop for Unlock {
            fn drop(&mut self) {
                unsafe {
                    (*self.0).unlock();
                }
            }
        }
        Unlock(lane)
    }
}

impl<A: Allocator> Drop for ViewLock<A> {
    fn drop(&mut self) {
        let mut ptr = *self.lanes.get_mut();
        if *self.switched.get_mut() {
            ptr = unsafe { ptr.sub(self.width) };
        }
        let Some(ptr) = NonNull::new(ptr) else {
            return;
        };

        let layout = core::alloc::Layout::array::<ViewLockLane>(self.width * 2).unwrap();
        unsafe {
            self.alloc.deallocate(ptr.cast(), layout);
        }
    }
}

#[cfg(test)]
mod test {
    use core::{iter, time::Duration};
    use std::thread;

    use super::*;

    #[test]
    #[ignore]
    fn bench_counter_spacing() {
        fn bench<const SPACING: usize>() -> Duration {
            let thread_count = 4;
            let counters = iter::repeat_with(|| [0; SPACING].map(AtomicUsize::new))
                .take(thread_count)
                .collect::<Vec<_>>();
            thread::scope(|tc| {
                let tasks = counters
                    .iter()
                    .map(|c| {
                        tc.spawn(|| {
                            let now = std::time::Instant::now();
                            for _ in 0..100_000_000 {
                                c[0].store(
                                    c[0].load(core::sync::atomic::Ordering::Acquire) + 1,
                                    core::sync::atomic::Ordering::Release,
                                );
                            }
                            now.elapsed()
                        })
                    })
                    .collect::<Vec<_>>();

                tasks
                    .into_iter()
                    .map(|t| t.join().unwrap())
                    .sum::<Duration>()
                    .checked_div(thread_count as u32)
                    .unwrap()
            })
        }

        println!("spacing 8: {:?}", bench::<8>());
        println!("spacing 4: {:?}", bench::<4>());
        println!("spacing 2: {:?}", bench::<2>());
        println!("spacing 1: {:?}", bench::<1>());
    }

    #[test]
    fn concurrent_pushes() {
        let threads = 4;
        let iters = if cfg!(miri) { 1 << 10 } else { 1 << 27 };
        let push_ratio = if cfg!(miri) { 1 } else { 1 << 7 };
        let div = iters / push_ratio;
        let mut base = ParallelBase::new(threads);

        let mut lanes = base.split().collect::<Vec<_>>();

        thread::scope(|tc| {
            for lane in &mut lanes {
                tc.spawn(|| {
                    let now = std::time::Instant::now();
                    for i in 0..iters {
                        if i & (push_ratio - 1) == 0 {
                            lane.push(i);
                        }
                    }
                    println!("lane: {:?}", now.elapsed());
                });
            }
        });

        for lane in &lanes {
            base.pull_from(lane);
        }

        for lane in &mut lanes {
            base.push_to(lane);
        }

        thread::scope(|tc| {
            for lane in &mut lanes {
                tc.spawn(|| {
                    let now = std::time::Instant::now();
                    for i in 0..iters {
                        let pr = ParRef::new(i & (threads - 1), i & (div - 1));
                        let val = *lane.get(pr);
                        if i & (push_ratio - 1) == 0 {
                            lane.push(val);
                        }
                    }
                    println!("lane: {:?}", now.elapsed());
                });
            }
        });
    }
}
