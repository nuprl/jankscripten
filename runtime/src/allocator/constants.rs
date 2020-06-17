/// If `p : *mut Tag` points to a tag, then `p + DATA_OFFSET` skips over the
/// pointer and references the data that follows it. Since the tag is 32-bits,
/// we calculate `p + 1` on a 32-bit architecture (Wasm) and `p + 2` on a
/// 64-bit architecture (amd64). This ensures that the data that follows
/// the tag is also naturally aligned.
#[cfg(target_pointer_width = "32")]
pub const DATA_OFFSET: usize = 1;

#[cfg(target_pointer_width = "64")]
pub const DATA_OFFSET: usize = 2;

#[cfg(target_pointer_width = "64")]
pub const ALIGNMENT: usize = 8;

#[cfg(target_pointer_width = "32")]
pub const ALIGNMENT: usize = 4;
