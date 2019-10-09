#![allow(dead_code)]

#[macro_use]
extern crate arrayref;

use std::slice;
use std::str;

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

// Accessors for the various State header fields.
impl State<'_> {
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

// Accessors for Level header fields.
impl Level<'_> {
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
    fn find_hyphen_values(&self, word: &str) -> Vec<u8> {
        let prep_word = ".".to_string() + word + ".";
        let word_bytes = prep_word.as_bytes();
        let mut values: Vec<u8> = vec![0; word_bytes.len()];
        if word.len() < self.lh_min() + self.rh_min() {
            return values;
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
        values
    }
}

#[derive(Debug)]
pub struct HyphDic<'a> {
    levels: Vec<Level<'a>>,
}

impl HyphDic<'_> {
    pub fn find_hyphen_values(&self, word: &str) -> Vec<u8> {
        self.levels[1].find_hyphen_values(word)
    }
    pub fn hyphenate_word(&self, word: &str, hyphchar: char) -> String {
        let level = self.levels[1];
        if word.len() < level.lh_min() + level.rh_min() {
            return word.to_string();
        }
        let values = level.find_hyphen_values(word);
        let mut result = word.to_string();
        for i in (level.lh_min() .. word.len() - level.rh_min() + 1).rev() {
            if values[i].is_odd() {
                result.insert(i, hyphchar);
            }
        }
        result
    }
}

pub fn load_hyphenation(mmap: &Mmap) -> HyphDic {
    let file_size = mmap.len() as usize;
    let num_levels = u32::from_le_bytes(*array_ref!(mmap, 0, 4));

    let mut dic = HyphDic { levels: Vec::new() };
    for i in 0..num_levels {
        let offset = u32::from_le_bytes(*array_ref!(mmap, (4 + 4 * i) as usize, 4)) as usize;
        let limit = if i == num_levels - 1 {
            file_size
        } else {
            u32::from_le_bytes(*array_ref!(mmap, (4 + 4 * i + 4) as usize, 4)) as usize
        };
        dic.levels.push(Level {
            data: mmap.get(offset..limit).unwrap()
        });
    }

//    for level in &dic.levels {
//        level.get_state(0).unwrap().deep_show("", &level);
//    }

    dic
}
