use std::sync::atomic::{AtomicU64, Ordering};

use smol_str::SmolStr;

pub type List<T> = imbl::Vector<T>;
pub type Map<T, U> = imbl::HashMap<T, U>;
pub type Set<T> = imbl::HashSet<T>;

pub fn gensym(pfx: &str) -> SmolStr {
    static COUNTER: AtomicU64 = AtomicU64::new(0);

    let ctr = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{pfx}{ctr}").into()
}
