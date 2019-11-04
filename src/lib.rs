/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#[macro_use]
extern crate arrayref;
extern crate memmap;

use std::slice;
use std::str;

use std::cmp::max;
use std::ffi::CStr;
use std::fs::File;
use std::os::raw::c_char;

use memmap::Mmap;

// 4-byte identification expected at beginning of a compiled dictionary file.
const MAGIC_NUMBER: [u8; 4] = [b'H', b'y', b'f', b'0'];

const INVALID_STRING_OFFSET: usize = 0xffff;
const INVALID_STATE_OFFSET: usize = 0xffffff;

// Transition actually holds a 24-bit new state offset and an 8-bit input byte
// to match. We will be interpreting byte ranges as Transition arrays (in the
// State::transitions() method below), so use repr(C) to ensure we have the
// memory layout we expect.
#[repr(C)]
#[derive(Debug,Copy,Clone)]
struct Transition(u8, u8, u8, u8);

impl Transition {
    fn new_state_offset(&self) -> usize {
        self.0 as usize + self.1 as usize * 0x100 + self.2 as usize * 0x10000
    }
    fn match_byte(&self) -> u8 {
        self.3
    }
}

// State is a reference to a slice of mmap'd data that begins with a fixed
// header, followed by an array of transitions. Total size of the data slice
// depends on the number of transitions in the state.
// There are two versions of State, a basic record that supports only simple
// hyphenation (no associated spelling change), and an extended version that
// adds the replacement-string fields to support spelling changes at the
// hyphenation point. Check is_extended() to know which version is present.
#[derive(Debug,Copy,Clone)]
struct State<'a> {
    data: &'a [u8],
}

impl State<'_> {
    // Accessors for the various State header fields.
    fn fallback_state(&self) -> usize {
        u32::from_le_bytes(*array_ref!(self.data, 0, 4)) as usize
    }
    fn match_string_offset(&self) -> usize {
        u16::from_le_bytes(*array_ref!(self.data, 4, 2)) as usize
    }
    fn num_transitions(&self) -> u8 {
        self.data[6]
    }
    fn is_extended(&self) -> bool {
        self.data[7] != 0
    }
    // Accessors that are only valid if is_extended() is true.
    #[allow(dead_code)]
    fn repl_string_offset(&self) -> usize {
        u16::from_le_bytes(*array_ref!(self.data, 8, 2)) as usize
    }
    #[allow(dead_code)]
    fn repl_index(&self) -> i8 {
        self.data[10] as i8
    }
    #[allow(dead_code)]
    fn repl_cut(&self) -> i8 {
        self.data[11] as i8
    }
    // Return the state's Transitions as a slice reference.
    fn transitions(&self) -> &[Transition] {
        let count = self.num_transitions() as usize;
        if count == 0 {
            return &[];
        }
        let transition_offset = if self.is_extended() { 12 } else { 8 };
        assert_eq!(self.data.len(), transition_offset + count * 4);
        let trans_ptr = &self.data[transition_offset] as *const u8 as *const Transition;
        // This is OK because we assert above that self.data.len() is large enough
        // to accommodate the expected number of Transitions.
        unsafe { slice::from_raw_parts(trans_ptr, count) }
    }
    // Look up the Transition for a given input byte, or None.
    fn transition_for(&self, b: u8) -> Option<Transition> {
        // The transitions array is sorted by match_byte() value, but there are
        // usually very few entries; benchmarking showed that using binary_search_by
        // here gave no benefit (possibly slightly slower).
        self.transitions().iter().copied().find(|t| t.match_byte() == b)
    }
    #[allow(dead_code)]
    fn deep_show(&self, prefix: &str, dic: &Level) {
        if self.match_string_offset() != INVALID_STRING_OFFSET {
            let match_string = dic.string_at_offset(self.match_string_offset());
            println!("{}match: {}", prefix, str::from_utf8(match_string).unwrap());
        }
        for t in self.transitions() {
            println!("{}{} ->", prefix, t.match_byte() as char);
            dic.get_state(t.new_state_offset()).unwrap().deep_show(&(prefix.to_owned() + "  "), &dic);
        }
    }
}

fn lig_length(trail_byte: u8) -> usize {
    // This is only called on valid UTF-8 where we already know trail_byte
    // must be >= 0x80.
    // Ligature lengths:       ff   fi   fl   ffi  ffl  long-st  st
    const LENGTHS: [u8; 7] = [ 2u8, 2u8, 2u8, 3u8, 3u8, 2u8,     2u8 ];
    if trail_byte > 0x86 {
        return 1;
    }
    LENGTHS[trail_byte as usize - 0x80] as usize
}

fn is_utf8_trail_byte(byte: u8) -> bool {
    (byte & 0xC0) == 0x80
}

fn is_ascii_digit(byte: u8) -> bool {
    byte <= 0x39 && byte >= 0x30
}

fn is_odd(byte: u8) -> bool {
    (byte & 0x01) == 0x01
}

// A hyphenation Level has a header followed by State records and packed string
// data. The total size of the slice depends on the number and size of the
// States and Strings it contains.
#[derive(Debug,Copy,Clone)]
struct Level<'a> {
    data: &'a [u8],
}

impl Level<'_> {
    // Accessors for Level header fields.
    fn state_data_base(&self) -> usize {
        u32::from_le_bytes(*array_ref!(self.data, 0, 4)) as usize
    }
    fn string_data_base(&self) -> usize {
        u32::from_le_bytes(*array_ref!(self.data, 4, 4)) as usize
    }
    fn nohyphen_string_offset(&self) -> usize {
        u16::from_le_bytes(*array_ref!(self.data, 8, 2)) as usize
    }
    #[allow(dead_code)]
    fn nohyphen_count(&self) -> u16 {
        u16::from_le_bytes(*array_ref!(self.data, 10, 2))
    }
    fn lh_min(&self) -> usize {
        max(1, self.data[12] as usize)
    }
    fn rh_min(&self) -> usize {
        max(1, self.data[13] as usize)
    }
    fn clh_min(&self) -> usize {
        max(1, self.data[14] as usize)
    }
    fn crh_min(&self) -> usize {
        max(1, self.data[15] as usize)
    }
    fn word_boundary_mins(&self) -> (usize, usize, usize, usize) {
        (self.lh_min(), self.rh_min(), self.clh_min(), self.crh_min())
    }
    // Strings are represented as offsets from the Level's string_data_base.
    // This returns a byte slice referencing the string at a given offset,
    // or an empty slice if invalid.
    fn string_at_offset(&self, offset: usize) -> &'_ [u8] {
        if offset == INVALID_STRING_OFFSET {
            return &[];
        }
        let string_base = self.string_data_base() as usize + offset;
        if string_base + 1 > self.data.len() {
            return &[];
        }
        let len = self.data[string_base] as usize;
        if string_base + 1 + len > self.data.len() {
            return &[];
        }
        self.data.get(string_base + 1 .. string_base + 1 + len).unwrap()
    }
    // The nohyphen field actually contains multiple NUL-separated substrings;
    // return them as a vector of individual byte slices.
    fn nohyphen(&self) -> Vec<&[u8]> {
        let string_offset = self.nohyphen_string_offset();
        let nohyph_str = self.string_at_offset(string_offset as usize);
        if nohyph_str.len() == 0 {
            return vec![];
        }
        nohyph_str.split(|&b| b == 0).collect()
    }
    // States are represented as an offset from the Level's state_data_base.
    // This returns the State at a given offset, or None if invalid.
    fn get_state(&self, offset: usize) -> Option<State> {
        if offset == INVALID_STATE_OFFSET {
            return None;
        }
        let state_base = self.state_data_base() + offset;
        if state_base + 8 > self.string_data_base() {
            return None;
        }
        let state_header = State {
            data: &self.data[state_base .. state_base + 8],
        };
        let length = if state_header.is_extended() { 12 } else { 8 } + 4 * state_header.num_transitions() as usize;
        if state_base + length > self.string_data_base() {
            return None;
        }
        Some(State{ data: &self.data[state_base .. state_base + length] })
    }
    // Sets hyphenation values (odd = potential break, even = no break) in values[],
    // and returns the change in the number of odd values present, so the caller can
    // keep track of the total number of potential breaks in the word.
    fn find_hyphen_values(&self, word: &str, values: &mut [u8], lh_min: usize, rh_min: usize) -> isize {
        // Bail out immediately if the word is too short to hyphenate.
        if word.len() < lh_min + rh_min {
            return 0;
        }
        let start_state = self.get_state(0);
        let mut st = start_state;
        let mut hyph_count = 0;
        for i in 0 .. word.len() + 2 {
            let b = if i == 0 || i == word.len() + 1 { b'.' } else { word.as_bytes()[i - 1] };
            loop {
                if st.is_none() {
                    st = start_state;
                    break;
                }
                let state = st.unwrap();
                if let Some(tr) = state.transition_for(b) {
                    st = self.get_state(tr.new_state_offset());
                    if let Some(state) = st {
                        let match_offset = state.match_string_offset();
                        if match_offset != INVALID_STRING_OFFSET {
                            if state.is_extended() {
                                debug_assert!(false, "extended hyphenation not supported by this function");
                            } else {
                                let match_str = self.string_at_offset(match_offset);
                                let offset = i + 1 - match_str.len();
                                assert!(offset + match_str.len() <= word.len() + 2);
                                for j in 0 .. match_str.len() {
                                    let index = offset + j;
                                    if index >= lh_min && index <= word.len() - rh_min {
                                        // lh_min and rh_min are guaranteed to be >= 1,
                                        // so this will not try to access outside values[].
                                        let old_value = values[index - 1];
                                        let value = match_str[j] - b'0';
                                        if value > old_value {
                                            if is_odd(old_value) != is_odd(value) {
                                                // Adjust hyph_count for the change we're making
                                                hyph_count += if is_odd(value) { 1 } else { -1 };
                                            }
                                            values[index - 1] = value;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
                st = self.get_state(state.fallback_state());
            }
        }

        // If the word was not purely ASCII, or if the word begins/ends with
        // digits, the use of lh_min and rh_min above may not have correctly
        // excluded enough positions, so we need to fix things up here.
        let mut index = 0;
        let mut count = 0;
        let word_bytes = word.as_bytes();
        // Handle lh_min
        while count < lh_min - 1 && index < word_bytes.len() {
            let byte = word_bytes[index];
            if is_odd(values[index]) {
                hyph_count -= 1;
            }
            values[index] = 0;
            if byte < 0x80 {
                index += 1;
                if is_ascii_digit(byte) {
                    continue; // ASCII digits don't count
                }
            } else if byte == 0xEF && index + 2 < word_bytes.len() && word_bytes[index + 1] == 0xAC {
                count += lig_length(word_bytes[index + 2]);
                if is_odd(values[index + 1]) {
                    hyph_count -= 1;
                }
                values[index + 1] = 0;
                if is_odd(values[index + 2]) {
                    hyph_count -= 1;
                }
                values[index + 2] = 0;
                index += 3;
                continue;
            } else {
                index += 1;
                while index < word_bytes.len() && is_utf8_trail_byte(word_bytes[index])  {
                    if is_odd(values[index]) {
                        hyph_count -= 1;
                    }
                    values[index] = 0;
                    index += 1;
                }
            }
            count += 1;
        }

        // Handle rh_min
        count = 0;
        index = word.len();
        while count < rh_min && index > 0 {
            index -= 1;
            let byte = word_bytes[index];
            if index < word.len() - 1 {
                if is_odd(values[index]) {
                    hyph_count -= 1;
                }
                values[index] = 0;
            }
            if byte < 0x80 {
                // Only count if not an ASCII digit
                if !is_ascii_digit(byte) {
                    count += 1;
                }
                continue;
            }
            if is_utf8_trail_byte(byte) {
                continue;
            }
            if byte == 0xEF && word_bytes[index + 1] == 0xAC {
                count += lig_length(word_bytes[index + 2]);
                continue;
            }
            count += 1;
        }

        hyph_count
    }
}

/// Hyphenation engine encapsulating a language-specific set of patterns (rules)
/// that identify possible break positions within a word.
pub struct Hyphenator<'a>(&'a [u8]);

impl Hyphenator<'_> {
    /// Return a Hyphenator that wraps the given buffer. This does *not* check
    /// that the given buffer is in fact a valid hyphenation table. Use
    /// is_valid_hyphenator() to determine whether it is usable.
    /// (Calling hyphenation methods on a Hyphenator that wraps arbitrary,
    /// unvalidated data is not unsafe, but may panic.)
    pub fn new(buffer: &[u8]) -> Hyphenator {
        Hyphenator(buffer)
    }

    // Internal implementation details
    fn magic_number(&self) -> &[u8] {
        &self.0[0 .. 4]
    }
    fn num_levels(&self) -> u32 {
        u32::from_le_bytes(*array_ref!(self.0, 4, 4))
    }
    fn level(&self, i: u32) -> Level {
        const FILE_HEADER_SIZE: u32 = 8; // 4-byte magic number, 4-byte count of levels
        let offset = u32::from_le_bytes(*array_ref!(self.0, (FILE_HEADER_SIZE + 4 * i) as usize, 4)) as usize;
        let limit = if i == self.num_levels() - 1 {
            self.0.len()
        } else {
            u32::from_le_bytes(*array_ref!(self.0, (FILE_HEADER_SIZE + 4 * i + 4) as usize, 4)) as usize
        };
        Level {
            data: &self.0[offset .. limit]
        }
    }

    /// Identify acceptable hyphenation positions in the given `word`.
    ///
    /// The caller-supplied `values` must be at least as long as the `word`.
    ///
    /// On return, any elements with an odd value indicate positions in the word
    /// after which a hyphen could be inserted.
    ///
    /// Returns the number of possible hyphenation positions that were found.
    ///
    /// # Panics
    /// If the given `values` slice is too small to hold the results.
    /// If the block of memory represented by `self.0` is not in fact a valid
    /// hyphenation dictionary, this function may panic with an overflow or
    /// array bounds violation.
    pub fn find_hyphen_values(&self, word: &str, values: &mut [u8]) -> isize {
        assert!(values.len() >= word.len());
        values.iter_mut().for_each(|x| *x = 0);
        let top_level = self.level(0);
        let (lh_min, rh_min, clh_min, crh_min) = top_level.word_boundary_mins();
        if word.len() < lh_min + rh_min {
            return 0;
        }
        let mut hyph_count = top_level.find_hyphen_values(word, values, lh_min, rh_min);
        let compound = hyph_count > 0;
        // Subsequent levels are applied to fragments between potential breaks
        // already found:
        for l in 1 .. self.num_levels() {
            let level = self.level(l);
            if hyph_count > 0 {
                let mut begin = 0;
                let mut lh = lh_min;
                for i in lh_min - 1 .. word.len() - rh_min {
                    if is_odd(values[i]) || (begin > 0 && i == word.len() - 1) {
                        if i > begin {
                            values[begin .. i].iter_mut().for_each(|x| {
                                if is_odd(*x) {
                                    hyph_count -= 1;
                                }
                                *x = 0;
                            });
                            hyph_count += level.find_hyphen_values(&word[begin .. i + 1],
                                                                   &mut values[begin .. i + 1],
                                                                   lh, crh_min);
                        }
                        begin = i + 1;
                        lh = clh_min;
                    }
                }
                if begin == 0 {
                    hyph_count += level.find_hyphen_values(word, values, lh_min, rh_min);
                } else if begin < word.len() {
                    hyph_count += level.find_hyphen_values(&word[begin .. word.len()],
                                                           &mut values[begin .. word.len()],
                                                           clh_min, rh_min);
                }
            } else {
                hyph_count += level.find_hyphen_values(word, values, lh_min, rh_min);
            }
        }

        // Only need to check nohyphen strings if top-level (compound) breaks were found.
        if compound && hyph_count > 0 {
            let nohyph = top_level.nohyphen();
            if nohyph.len() > 0 {
                for i in lh_min .. word.len() - rh_min + 1 {
                    if is_odd(values[i - 1]) {
                        for nh in &nohyph {
                            if i + nh.len() <= word.len() && *nh == &word.as_bytes()[i .. i + nh.len()] {
                                values[i - 1] = 0;
                                hyph_count -= 1;
                                break;
                            }
                            if nh.len() <= i && *nh == &word.as_bytes()[i - nh.len() .. i] {
                                values[i - 1] = 0;
                                hyph_count -= 1;
                                break;
                            }
                        }
                    }
                }
            }
        }

        hyph_count
    }

    /// Generate the hyphenated form of a `word` by inserting the given `hyphen_char`
    /// at each valid break position.
    ///
    /// # Panics
    /// If the block of memory represented by `self` is not in fact a valid
    /// hyphenation dictionary, this function may panic with an overflow or
    /// array bounds violation.
    pub fn hyphenate_word(&self, word: &str, hyphchar: char) -> String {
        let mut values = vec![0u8; word.len()];
        let hyph_count = self.find_hyphen_values(word, &mut values);
        let mut result = word.to_string();
        let mut n = 0;
        for i in (0 .. word.len()).rev() {
            if is_odd(values[i]) {
                result.insert(i + 1, hyphchar);
                n += 1;
            }
        }
        debug_assert_eq!(n, hyph_count);
        result
    }

    /// Check if the block of memory looks like it could be a valid hyphenation
    /// table.
    pub fn is_valid_hyphenator(&self) -> bool {
        // Size must be at least 4 bytes for magic_number + 4 bytes num_levels;
        // smaller than this cannot be safely inspected.
        if self.0.len() < 8 {
            return false;
        }
        if self.magic_number() != MAGIC_NUMBER {
            return false;
        }
        // For each level, there's a 4-byte offset in the header, and the level
        // has its own 16-byte header, so we can check a minimum size again here.
        let num_levels = self.num_levels();
        if self.0.len() < 8 + 16 * num_levels as usize {
            return false;
        }
        // Check that state_data_base and string_data_base for each hyphenation
        // level are within range.
        for l in 0 .. num_levels {
            let level = self.level(l);
            if level.state_data_base() < 16 ||
                   level.state_data_base() > level.string_data_base() ||
                   level.string_data_base() > level.data.len() {
                return false;
            }
            // TODO: consider doing more extensive validation of states and
            // strings within the level?
        }
        // It's still possible the dic is internally broken, but at least it's
        // worth trying to use it!
        true
    }
}

/// Load the compiled hyphenation file at `dic_path`, if present.
///
/// Returns `None` if the specified file cannot be opened or mapped,
/// otherwise returns a `memmap::Mmap` mapping the file.
///
/// This does *not* check the validity of the file contents!
pub fn load_file(dic_path: &str) -> Option<Mmap> {
    let file = File::open(dic_path).ok()?;
    let dic = unsafe { Mmap::map(&file) }.ok()?;
    let hyph = Hyphenator(&*dic);
    if hyph.is_valid_hyphenator() {
        return Some(dic);
    }
    None
}

/// Opaque type representing a hyphenation dictionary loaded from a file,
/// for use in FFI function signatures.
pub struct HyphDic;

/// C-callable function to load a hyphenation dictionary from a file at `path`.
///
/// Returns null on failure.
///
/// This does not fully validate that the file contains usable hyphenation
/// data, it only opens the file (read-only) and mmap's it into memory, and
/// does some minimal sanity-checking that it *might* be valid.
///
/// The returned `HyphDic` must be released with `mapped_hyph_free_dictionary`.
///
/// # Safety
/// The given `path` must be a valid pointer to a NUL-terminated (C-style)
/// string.
#[no_mangle]
pub extern "C" fn mapped_hyph_load_dictionary(path: *const c_char) -> *const HyphDic {
    let path_str = match unsafe { CStr::from_ptr(path) }.to_str() {
        Ok(str) => str,
        Err(_) => return std::ptr::null(),
    };
    let hyph = Box::new(match load_file(path_str) {
        Some(dic) => dic,
        _ => return std::ptr::null(),
    });
    Box::into_raw(hyph) as *const HyphDic
}

/// C-callable function to free a hyphenation dictionary
/// that was loaded by `mapped_hyph_load_dictionary`.
///
/// # Safety
/// The `dic` parameter must be a `HyphDic` pointer obtained from
/// `mapped_hyph_load_dictionary`, and not previously freed.
#[no_mangle]
pub extern "C" fn mapped_hyph_free_dictionary(dic: *mut HyphDic) {
    unsafe { Box::from_raw(dic) };
}

/// C-callable function to find hyphenation values for a given `word`,
/// using a dictionary loaded via `mapped_hyph_load_dictionary`.
///
/// The `word` must be UTF-8-encoded, and is `word_len` bytes (not characters)
/// long.
///
/// Caller must supply the `hyphens` output buffer for results; its size is
/// given in `hyphens_len`.
/// It should be at least `word_len` elements long.
///
/// Returns -1 if `word` is not valid UTF-8, or the output `hyphens` buffer is
/// too small.
/// Otherwise returns the number of potential hyphenation positions found.
///
/// # Panics
/// This function may panic if the given dictionary is not valid.
///
/// # Safety
/// The `dic` parameter must be a `HyphDic` pointer obtained from
/// `mapped_hyph_load_dictionary`.
///
/// The `word` and `hyphens` parameter must be valid pointers to memory buffers
/// of at least the respective sizes `word_len` and `hyphens_len`.
#[no_mangle]
pub extern "C" fn mapped_hyph_find_hyphen_values_dic(dic: *const HyphDic,
                                                     word: *const c_char, word_len: u32,
                                                     hyphens: *mut u8, hyphens_len: u32) -> i32 {
    if word_len > hyphens_len {
        return -1;
    }
    let word_str = match str::from_utf8(unsafe {
            slice::from_raw_parts(word as *const u8, word_len as usize) } ) {
        Ok(word) => word,
        Err(_) => return -1,
    };
    let hyphen_buf = unsafe { slice::from_raw_parts_mut(hyphens, hyphens_len as usize) };
    let hyph = Hyphenator(unsafe { &*(dic as *const Mmap) });
    hyph.find_hyphen_values(word_str, hyphen_buf) as i32
}

/// C-callable function to find hyphenation values for a given `word`,
/// using a dictionary loaded and owned by the caller.
///
/// The dictionary is supplied as a raw memory buffer `dic_buf` of size
/// `dic_len`.
///
/// The `word` must be UTF-8-encoded, and is `word_len` bytes (not characters)
/// long.
///
/// Caller must supply the `hyphens` output buffer for results; its size is
/// given in `hyphens_len`.
/// It should be at least `word_len` elements long.
///
/// Returns -1 if `word` is not valid UTF-8, or the output `hyphens` buffer is
/// too small.
/// Otherwise returns the number of potential hyphenation positions found.
///
/// # Panics
/// This function may panic if the given dictionary is not valid.
///
/// # Safety
/// The `dic_buf` parameter must be a valid pointer to a memory block of size
/// at least `dic_len`.
///
/// The `word` and `hyphens` parameter must be valid pointers to memory buffers
/// of at least the respective sizes `word_len` and `hyphens_len`.
#[no_mangle]
pub extern "C" fn mapped_hyph_find_hyphen_values_raw(dic_buf: *const u8, dic_len: u32,
                                                     word: *const c_char, word_len: u32,
                                                     hyphens: *mut u8, hyphens_len: u32) -> i32 {
    if word_len > hyphens_len {
        return -1;
    }
    let word_str = match str::from_utf8(unsafe {
            slice::from_raw_parts(word as *const u8, word_len as usize) } ) {
        Ok(word) => word,
        Err(_) => return -1,
    };
    let hyphen_buf = unsafe { slice::from_raw_parts_mut(hyphens, hyphens_len as usize) };
    let dic = Hyphenator(unsafe { slice::from_raw_parts(dic_buf, dic_len as usize) });
    dic.find_hyphen_values(word_str, hyphen_buf) as i32
}

/// C-callable function to check if a given memory buffer `dic_buf` of size
/// `dic_len` is potentially usable as a hyphenation dictionary.
///
/// Returns `true` if the given memory buffer looks like it may be a valid
/// hyphenation dictionary, `false` if it is clearly not usable.
#[no_mangle]
pub extern "C" fn mapped_hyph_is_valid_hyphenator(dic_buf: *const u8, dic_len: u32) -> bool {
    if dic_buf == 0 as *const u8 {
        return false;
    }
    let dic = Hyphenator(unsafe { slice::from_raw_parts(dic_buf, dic_len as usize) });
    dic.is_valid_hyphenator()
}
