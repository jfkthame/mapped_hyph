#![allow(dead_code)]
#![feature(test)]

#[macro_use]
extern crate arrayref;
extern crate test;

use std::fs::File;
use std::slice;
use std::str;

use std::ffi::CStr;
use std::os::raw::c_char;

use memmap::Mmap;
use num::Integer;

const INVALID_STRING_OFFSET: usize = 0xffff;
const INVALID_STATE_OFFSET: usize = 0xffffff;

// Transition actually holds a 24-bit new state offset and an 8-bit input byte
// to match. We will be interpreting byte ranges as Transition arrays (in the
// State::transitions() method below), so use repr(C) to ensure we have the
// memory layout we expect.
#[repr(C)]
#[derive(Debug,Clone,Copy)]
struct Transition ( u8, u8, u8, u8 );

impl Transition {
    fn new_state_offset(&self) -> usize {
        self.0 as usize + self.1 as usize * 0x100 + self.2 as usize * 0x10000
    }
    fn match_byte(&self) -> u8 {
        self.3
    }
}

// State is a reference to a slice of mmap'd data that begins with a fixed
// header, followed by an array of transitions. Size of the data slice depends
// on the number of transitions in the state.
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
    fn repl_string_offset(&self) -> usize {
        u16::from_le_bytes(*array_ref!(self.data, 6, 2)) as usize
    }
    fn repl_index(&self) -> i8 {
        self.data[8] as i8
    }
    fn repl_cut(&self) -> i8 {
        self.data[9] as i8
    }
    fn num_transitions(&self) -> u8 {
        self.data[10]
    }
    // Return the state's Transitions as a slice reference.
    fn transitions(&self) -> &[Transition] {
        let count = self.num_transitions() as usize;
        if count == 0 {
            return &[];
        }
        assert!(self.data.len() == 12 + count * 4);
        let trans_ptr = &self.data[12] as *const u8 as *const Transition;
        unsafe { slice::from_raw_parts(trans_ptr, count) }
    }
    // Look up the Transition for a given input byte, or None.
    fn transition_for(&self, b: u8) -> Option<Transition> {
        // TODO: investigate whether binary search is worthwhile here. (The
        // transitions array is sorted by match_byte() value.)
        for t in self.transitions() {
            if t.match_byte() == b {
                return Some(*t);
            }
        }
        None
    }
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

// A hyphenation Level has a header followed by State records and packed string
// data. The total size of the slice depends on the number and size of the
// States and Strings it contains.
#[derive(Debug,Copy,Clone)]
struct Level<'a> {
    data: &'a [u8],
}

impl Level<'_> {
    // Accessors for Level header fields.
    fn num_states(&self) -> u32 {
        u32::from_le_bytes(*array_ref!(self.data, 0, 4))
    }
    fn state_data_base(&self) -> usize {
        16 + 4 * self.num_states() as usize
    }
    fn string_data_base(&self) -> usize {
        u32::from_le_bytes(*array_ref!(self.data, 4, 4)) as usize
    }
    fn nohyphen_string_offset(&self) -> usize {
        u16::from_le_bytes(*array_ref!(self.data, 8, 2)) as usize
    }
    fn nohyphen_count(&self) -> u16 {
        u16::from_le_bytes(*array_ref!(self.data, 10, 2))
    }
    fn lh_min(&self) -> usize {
        self.data[12] as usize
    }
    fn rh_min(&self) -> usize {
        self.data[13] as usize
    }
    fn clh_min(&self) -> usize {
        self.data[14] as usize
    }
    fn crh_min(&self) -> usize {
        self.data[15] as usize
    }
    // Strings are represented as offsets from the Level's string_data_base.
    // This returns a byte slice referencing the string at a given offset,
    // or an empty slice if invalid.
    fn string_at_offset(&self, offset: usize) -> &'_ [u8] {
        if offset == INVALID_STRING_OFFSET {
            return &[];
        }
        let string_base = self.string_data_base() as usize + offset;
        let len = self.data[string_base] as usize;
        self.data.get(string_base + 1 .. string_base + 1 + len).unwrap()
    }
    // The nohyphen field is actuall a string that contains multiple NUL-
    // separated substrings; return them as a vector of individual strings.
    fn nohyphen(&self) -> Vec<&str> {
        str::from_utf8(self.string_at_offset(self.nohyphen_string_offset() as usize)).unwrap().split('\0').collect()
    }
    // States are represented as an offset from the Level's state_data_base.
    // This returns the State at a given offset, or None if invalid.
    fn get_state(&self, offset: usize) -> Option<State> {
        if offset == INVALID_STATE_OFFSET {
            return None;
        }
        assert!(offset < self.string_data_base());
        let base = self.state_data_base();
        let state_header = State {
            data: &self.data[base+offset..base+offset+12],
        };
        let state_len = (12 + 4 * state_header.num_transitions()) as usize;
        Some(State {
            data: &self.data[base+offset..base+offset+state_len],
        })
    }
    fn find_hyphen_values(&self, word: &str, values: &mut [u8]) {
        values.iter_mut().for_each(|x| *x = 0);
        let prep_word = ".".to_string() + word + ".";
        let word_bytes = prep_word.as_bytes();
        if word.len() < self.lh_min() + self.rh_min() {
            return;
        }
        let start_state = self.get_state(0);
        let mut state = start_state;
        for i in 0..word_bytes.len() {
            let b = word_bytes[i];
            loop {
                if state.is_none() {
                    state = start_state;
                    break;
                }
                let t = state.unwrap().transition_for(b);
                if t.is_some() {
                    state = self.get_state(t.unwrap().new_state_offset());
                    if state.is_some() &&
                            state.unwrap().match_string_offset() != INVALID_STRING_OFFSET &&
                            state.unwrap().repl_string_offset() == INVALID_STRING_OFFSET {
                        let match_str = self.string_at_offset(state.unwrap().match_string_offset());
                        let offset = i + 1 - match_str.len();
                        assert!(offset + match_str.len() <= word_bytes.len());
                        for j in 0 .. match_str.len() {
                            if match_str[j] - b'0' > values[offset + j] {
                                values[offset + j] = match_str[j] - b'0';
                            }
                        }
                    }
                    break;
                }
                state = self.get_state(state.unwrap().fallback_state());
            }
        }
    }
}

#[derive(Debug)]
pub struct HyphDic {
    mmap: Mmap,
}

impl HyphDic {
    fn level(&self, i: u32) -> Level {
        let file_size = self.mmap.len() as usize;
        let num_levels = u32::from_le_bytes(*array_ref!(self.mmap, 0, 4));
        let offset = u32::from_le_bytes(*array_ref!(self.mmap, (4 + 4 * i) as usize, 4)) as usize;
        let limit = if i == num_levels - 1 {
            file_size
        } else {
            u32::from_le_bytes(*array_ref!(self.mmap, (4 + 4 * i + 4) as usize, 4)) as usize
        };
        Level {
            data: self.mmap.get(offset..limit).unwrap()
        }
    }
    pub fn find_hyphen_values(&self, word: &str, values: &mut [u8]) {
        self.level(1).find_hyphen_values(word, values);
    }
    pub fn hyphenate_word(&self, word: &str, hyphchar: char) -> String {
        let level = self.level(1);
        if word.len() < level.lh_min() + level.rh_min() {
            return word.to_string();
        }
        let mut values: Vec<u8> = vec![0; word.len() + 2];
        level.find_hyphen_values(word, &mut values);
        let mut result = word.to_string();
        for i in (level.lh_min() .. word.len() - level.rh_min() + 1).rev() {
            if values[i].is_odd() {
                result.insert(i, hyphchar);
            }
        }
        result
    }
}

pub fn load(dic_path: &str) -> Option<HyphDic> {
    let file = match File::open(dic_path) {
                Err(_) => return None,
                Ok(file) => file,
            };
    let dic = HyphDic {
        mmap: match unsafe { Mmap::map(&file)} {
            Err(_) => return None,
            Ok(mmap) => mmap,
        }
    };
    Some(dic)
}

// C-callable function to load a hyphenation dictionary; returns null on failure.
// `path` must be a valid UTF-8 string, or it will panic!
#[no_mangle]
pub extern "C" fn load_hyphenation(path: *const c_char) -> *const HyphDic {
    let path_str = match unsafe { CStr::from_ptr(path) }.to_str() {
        Ok(str) => str,
        Err(_) => return std::ptr::null(),
    };
    let hyph = Box::new(match load(path_str) {
        Some(dic) => dic,
        _ => return std::ptr::null(),
    });
    Box::into_raw(hyph)
}

// C-callable function to free a hyphenation dictionary loaded by load_hyphenation.
#[no_mangle]
pub extern "C" fn free_hyphenation(hyph_ptr: *mut HyphDic) {
    unsafe { Box::from_raw(hyph_ptr) };
}

// C-callable function to find hyphenation values for a word.
// Caller must supply the `hyphens` output buffer for results.
// **NOTE** that the `hyphens` buffer must be at least `word_len + 2` elements long.
// Returns true on success; false if word is not valid UTF-8 or output buffer too small.
#[no_mangle]
pub extern "C" fn find_hyphen_values(dic: &HyphDic, word: *const c_char, word_len: u32,
                                     hyphens: *mut u8, hyphens_len: u32) -> bool {
    if word_len + 2 > hyphens_len {
        return false;
    }
    let word_str = match str::from_utf8(unsafe { slice::from_raw_parts(word as *const u8, word_len as usize) } ) {
        Ok(word) => word,
        Err(_) => return false,
    };
    let hyphen_buf = unsafe { slice::from_raw_parts_mut(hyphens, hyphens_len as usize) };
    dic.find_hyphen_values(word_str, hyphen_buf);
    true
}

#[cfg(test)]
mod tests {
    #[test]
    fn basic_tests() {
        let dic_path = "hyph_en_US.hyf";
        let hyph = match super::load(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        assert_eq!(hyph.hyphenate_word("haha", '-'), "haha");
        assert_eq!(hyph.hyphenate_word("hahaha", '-'), "ha-haha");
        assert_eq!(hyph.hyphenate_word("photo", '-'), "photo");
        assert_eq!(hyph.hyphenate_word("photograph", '-'), "pho-to-graph");
        assert_eq!(hyph.hyphenate_word("photographer", '-'), "pho-tog-ra-pher");
        assert_eq!(hyph.hyphenate_word("photographic", '-'), "pho-to-graphic");
        assert_eq!(hyph.hyphenate_word("photographical", '-'), "pho-to-graph-i-cal");
        assert_eq!(hyph.hyphenate_word("photographically", '-'), "pho-to-graph-i-cally");
        assert_eq!(hyph.hyphenate_word("supercalifragilisticexpialidocious", '-'), "su-per-cal-ifrag-ilis-tic-ex-pi-ali-do-cious");
    }

    #[bench]
    fn bench_words(b: &mut test::Bencher) {
        b.iter(|| {
            let dic_path = "hyph_en_US.hyf";
            let hyph = match super::load(dic_path) {
                Some(dic) => dic,
                _ => panic!("failed to load dictionary {}", dic_path),
            };
            let words_file = match File::open("/usr/share/dict/words") {
                Err(why) => panic!("couldn't open file: {}", why),
                Ok(file) => file,
            };
            use std::fs::File;
            use std::io::{BufRead,BufReader};
            let reader = BufReader::new(words_file);
            let mut values: Vec<u8> = vec![0; 1000];
            for line in reader.lines() {
                let line = line.unwrap(); // Ignore errors.
                let _result = hyph.find_hyphen_values(&line, &mut values);
                //println!("{}", result);
            }
        });
    }
}
