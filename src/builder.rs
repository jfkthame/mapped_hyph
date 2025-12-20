// Copyright 2019-2020 Mozilla Foundation. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Functions to compile human-readable patterns into a mapped_hyph
//! flattened representation of the hyphenation state machine.

use std::convert::TryInto;
use std::io::{BufRead, BufReader, Error, ErrorKind, Read, Write};

use bumpalo::Bump;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

// Wrap a FxHashMap so that we can implement the Hash trait.
#[derive(PartialEq, Eq, Hash, Clone)]
struct TransitionMap(SmallVec<[(u8, i32); 2]>);

impl TransitionMap {
    fn new() -> TransitionMap {
        TransitionMap(SmallVec::new())
    }

    fn insert(&mut self, ch: u8, state_num: i32) -> Option<i32> {
        match self.0.binary_search_by_key(&ch, |(c, _)| *c) {
            Ok(i) => Some(std::mem::replace(&mut self.0[i].1, state_num)),
            Err(i) => {
                self.0.insert(i, (ch, state_num));
                None
            }
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct State<'a> {
    match_string: Option<&'a [u8]>,
    repl_string: Option<&'a [u8]>,
    repl_index: i32,
    repl_cut: i32,
    fallback_state: i32,
    transitions: TransitionMap,
}

impl<'a> State<'a> {
    fn new() -> Self {
        State {
            match_string: None,
            repl_string: None,
            repl_index: -1,
            repl_cut: -1,
            fallback_state: -1,
            transitions: TransitionMap::new(),
        }
    }
}

/// Structures returned by the read_dic_file() function;
/// array of these can then be passed to write_hyf_file()
/// to create the flattened output.
struct LevelBuilder<'a> {
    bump: &'a Bump,
    states: Vec<State<'a>>,
    str_to_state: FxHashMap<&'a [u8], i32>,
    encoding: Option<String>,
    nohyphen: Option<String>,
    lh_min: u8,
    rh_min: u8,
    clh_min: u8,
    crh_min: u8,
}

impl<'a> LevelBuilder<'a> {
    fn new(bump: &'a Bump) -> Self {
        let mut result = Self {
            bump,
            states: Vec::<State>::new(),
            str_to_state: FxHashMap::default(),
            encoding: None,
            nohyphen: None,
            lh_min: 0,
            rh_min: 0,
            clh_min: 0,
            crh_min: 0,
        };
        // Initialize the builder with an empty start state.
        result.str_to_state.insert(&[], 0);
        result.states.push(State::new());
        result
    }

    fn find_state_number_for(&mut self, text: &'a [u8]) -> i32 {
        let count = self.states.len() as i32;
        let index = *self.str_to_state.entry(text).or_insert(count);
        if index == count {
            self.states.push(State::new());
        }
        index
    }

    fn add_pattern(&mut self, pattern: &str) {
        let mut bytes = pattern.as_bytes();
        let mut text = SmallVec::<[u8; 20]>::with_capacity(bytes.len());
        let mut digits = SmallVec::<[u8; 20]>::with_capacity(bytes.len() + 1);
        let mut repl_str: Option<&[u8]> = None;
        let mut repl_index = 0;
        let mut repl_cut = 0;

        // Check for replacement rule (non-standard hyphenation spelling change).
        if let Some(slash) = bytes.iter().position(|x| *x == b'/') {
            let (before_slash, slash_and_after) = bytes.split_at(slash);
            bytes = before_slash;
            let mut it = slash_and_after[1..].split(|x| *x == b',');
            if let Some(repl) = it.next() {
                repl_str = Some(self.bump.alloc_slice_copy(repl));
            }
            if let Some(num) = it.next() {
                repl_index = std::str::from_utf8(num).unwrap().parse::<i32>().unwrap() - 1;
            }
            if let Some(num) = it.next() {
                repl_cut = std::str::from_utf8(num).unwrap().parse::<i32>().unwrap();
            }
        }

        // Separate the input pattern into parallel arrays of text (bytes) and digits.
        let mut got_digit = false;
        for byte in bytes {
            if *byte <= b'9' && *byte >= b'0' {
                if got_digit {
                    warn!("invalid pattern \"{}\": consecutive digits", pattern);
                    return;
                }
                digits.push(*byte);
                got_digit = true;
            } else {
                text.push(*byte);
                if got_digit {
                    got_digit = false;
                } else {
                    digits.push(b'0');
                }
            }
        }
        if !got_digit {
            digits.push(b'0');
        }

        let mut digits = &digits[..];
        if repl_str.is_none() {
            // Optimize away leading zeroes from the digits array.
            while !digits.is_empty() && digits[0] == b'0' {
                digits = &digits[1..];
            }
        } else {
            // Convert repl_index and repl_cut from Unicode char to byte indexing.
            let start = if text[0] == b'.' { 1 } else { 0 };
            if start == 1 {
                if digits[0] != b'0' {
                    warn!(
                        "invalid pattern \"{}\": unexpected digit before start of word",
                        pattern
                    );
                    return;
                }
                digits = &digits[1..];
            }
            let word = std::str::from_utf8(&text[start..]).unwrap();
            let mut chars: Vec<_> = word.char_indices().collect();
            chars.push((word.len(), '.'));
            repl_cut = chars[(repl_index + repl_cut) as usize].0 as i32
                - chars[repl_index as usize].0 as i32;
            repl_index = chars[repl_index as usize].0 as i32;
        }

        // Create the new state, or add pattern into an existing state
        // (which should not already have a match_string).
        let mut text: &[u8] = self.bump.alloc_slice_copy(&text);
        let mut state_num = self.find_state_number_for(text);
        let state = &mut self.states[state_num as usize];
        if state.match_string.is_some() {
            warn!("duplicate pattern \"{}\" discarded", pattern);
            return;
        }
        if !digits.is_empty() {
            state.match_string = Some(self.bump.alloc_slice_copy(digits));
        }
        if repl_str.is_some() {
            state.repl_string = repl_str;
            state.repl_index = repl_index;
            state.repl_cut = repl_cut;
        }

        // Set up prefix transitions, inserting additional states as needed.
        while let Some((ch, trunc_text)) = text.split_last() {
            let last_state = state_num;
            text = trunc_text;
            state_num = self.find_state_number_for(text);
            if let Some(exists) = self.states[state_num as usize]
                .transitions
                .insert(*ch, last_state)
            {
                assert_eq!(
                    exists, last_state,
                    "overwriting existing transition at pattern \"{}\"",
                    pattern
                );
                break;
            }
        }
    }

    fn merge_duplicate_states(&mut self) {
        // We loop here because when we eliminate a duplicate, and update the transitons
        // that referenced it, we may thereby create new duplicates that another pass
        // will find and compress further.
        loop {
            let orig_len = self.states.len();
            // Used to map State records to the (first) index at which they occur.
            let mut state_to_index = FxHashMap::<&State, i32>::default();
            // Mapping of old->new state indexes, and whether each old state is
            // a duplicate that should be dropped.
            let mut mappings = Vec::<(i32, bool)>::with_capacity(orig_len);
            let mut next_new_index: i32 = 0;
            for index in 0..self.states.len() {
                // Find existing index for this state, or allocate the next new index to it.
                let new_index = *state_to_index
                    .entry(&self.states[index])
                    .or_insert(next_new_index);
                // Record the mapping, and whether the state was a duplicate.
                mappings.push((new_index, new_index != next_new_index));
                // If we used next_new_index for this state, increment it.
                if new_index == next_new_index {
                    next_new_index += 1;
                }
            }
            // If we didn't find any duplicates, next_new_index will have kept pace with
            // index, so we know we're finished.
            if next_new_index as usize == self.states.len() {
                break;
            }
            // Iterate over all the states, either deleting them or updating indexes
            // according to the mapping we created; then repeat the search.
            let mut index = 0;
            self.states.retain_mut(|state| {
                let (new_state, is_duplicate) = mappings[index];
                if !is_duplicate {
                    if state.fallback_state != -1 {
                        state.fallback_state = new_state;
                    }
                    for (_, dest_state) in &mut state.transitions.0 {
                        *dest_state = mappings[*dest_state as usize].0;
                    }
                }
                index += 1;
                !is_duplicate
            });
        }
    }

    fn flatten(&self) -> Vec<u8> {
        // Calculate total space needed for state data, and build the state_to_offset table.
        let mut state_data_size = 0;
        let mut state_to_offset = Vec::<usize>::with_capacity(self.states.len());
        for state in &self.states {
            state_to_offset.push(state_data_size);
            state_data_size += if state.repl_string.is_some() { 12 } else { 8 };
            state_data_size += state.transitions.0.len() * 4;
        }

        // Helper to map a state index to its offset in the final data block.
        let get_state_offset_for = |state_index: i32| -> u32 {
            if state_index < 0 {
                return super::INVALID_STATE_OFFSET;
            }
            state_to_offset[state_index as usize] as u32
        };

        // Helper to map a byte string to its offset in the final data block, and
        // store the bytes into string_data unless using an already-existing string.
        let mut string_to_offset = FxHashMap::<&'a [u8], usize>::default();
        let mut string_data = Vec::<u8>::new();
        let mut get_string_offset_for = |bytes: Option<&'a [u8]>| -> u16 {
            let Some(bytes) = bytes else {
                return super::INVALID_STRING_OFFSET;
            };
            assert!(bytes.len() < 256);
            let new_offset = string_data.len();
            let offset = *string_to_offset.entry(bytes).or_insert(new_offset);
            if offset == new_offset {
                string_data.push(bytes.len() as u8);
                string_data.extend_from_slice(bytes.as_ref());
            }
            offset.try_into().unwrap()
        };

        // Handle nohyphen string list if present, converting comma separators to NULs
        // and trimming any surplus whitespace.
        let mut nohyphen_string_offset: u16 = super::INVALID_STRING_OFFSET;
        let mut nohyphen_count: u16 = 0;
        if self.nohyphen.is_some() {
            let nohyphen_strings: Vec<_> = self
                .nohyphen
                .as_ref()
                .unwrap()
                .split(',')
                .map(|x| x.trim())
                .collect();
            nohyphen_count = nohyphen_strings.len().try_into().unwrap();
            let nohyphen_string = nohyphen_strings.join("\0");
            let no_hyphen_str = self.bump.alloc_slice_copy(nohyphen_string.as_bytes());
            nohyphen_string_offset = get_string_offset_for(Some(no_hyphen_str));
        }

        let mut state_data = Vec::<u8>::with_capacity(state_data_size);
        for state in &self.states {
            state_data.extend_from_slice(&get_state_offset_for(state.fallback_state).to_le_bytes());
            state_data.extend_from_slice(&get_string_offset_for(state.match_string).to_le_bytes());
            state_data.push(state.transitions.0.len() as u8);
            // Determine whether to use an extended state record, and if so add the
            // replacement string and index fields.
            if state.repl_string.is_none() {
                state_data.push(0);
            } else {
                state_data.push(1);
                state_data
                    .extend_from_slice(&get_string_offset_for(state.repl_string).to_le_bytes());
                state_data.push(state.repl_index as u8);
                state_data.push(state.repl_cut as u8);
            }
            // Collect transitions into an array so we can sort them.
            for (key, value) in &state.transitions.0 {
                let dest_state_offset = get_state_offset_for(*value);
                // Destination state offset is stored as a 24-bit value, so we do this manually.
                state_data.push((dest_state_offset & 0xff) as u8);
                state_data.push(((dest_state_offset >> 8) & 0xff) as u8);
                state_data.push(((dest_state_offset >> 16) & 0xff) as u8);
                state_data.push(*key);
            }
        }
        assert_eq!(state_data.len(), state_data_size);

        // Pad string data to a 4-byte boundary
        while string_data.len() & 3 != 0 {
            string_data.push(0);
        }

        let total_size = super::LEVEL_HEADER_SIZE + state_data_size + string_data.len();
        let mut result = Vec::<u8>::with_capacity(total_size);

        let state_data_base: u32 = super::LEVEL_HEADER_SIZE as u32;
        let string_data_base: u32 = state_data_base + state_data_size as u32;

        result.extend_from_slice(&state_data_base.to_le_bytes());
        result.extend_from_slice(&string_data_base.to_le_bytes());
        result.extend_from_slice(&nohyphen_string_offset.to_le_bytes());
        result.extend_from_slice(&nohyphen_count.to_le_bytes());
        result.push(self.lh_min);
        result.push(self.rh_min);
        result.push(self.clh_min);
        result.push(self.crh_min);

        result.extend_from_slice(&state_data);
        result.extend_from_slice(&string_data);

        assert_eq!(result.len(), total_size);

        result
    }
}

/// Read a libhyphen-style pattern file and create the corresponding state
/// machine transitions, etc.
/// The returned Vec can be passed to write_hyf_file() to generate a flattened
/// representation of the state machine in mapped_hyph's binary format.
fn read_dic_file<T: Read>(
    dic_file: T,
    bump: &Bump,
    compress: bool,
) -> Result<Vec<LevelBuilder<'_>>, &'static str> {
    let mut reader = BufReader::new(dic_file);

    let mut builders = Vec::<LevelBuilder>::new();
    builders.push(LevelBuilder::new(bump));
    let mut builder = &mut builders[0];

    let mut line = String::new();
    let mut index = 0;
    loop {
        line.clear();
        index += 1;
        if reader.read_line(&mut line).unwrap() == 0 {
            break;
        }
        let mut trimmed = line.trim();
        // Strip comments.
        if let Some(i) = trimmed.find('%') {
            trimmed = trimmed[..i].trim();
        }
        // Ignore empty lines.
        if trimmed.is_empty() {
            continue;
        }
        // Uppercase indicates keyword rather than pattern.
        if trimmed.as_bytes()[0] >= b'A' && trimmed.as_bytes()[0] <= b'Z' {
            // First line is encoding; we only support UTF-8.
            if builder.encoding.is_none() {
                if trimmed != "UTF-8" {
                    return Err("Only UTF-8 patterns are accepted!");
                };
                builder.encoding = Some(trimmed.to_string());
                continue;
            }
            // Check for valid keyword-value pairs.
            if trimmed.contains(' ') {
                let parts: Vec<&str> = trimmed.split(' ').collect();
                if parts.len() != 2 {
                    warn!("unrecognized keyword/values: {}", trimmed);
                    continue;
                }
                let keyword = parts[0];
                let value = parts[1];
                match keyword {
                    "LEFTHYPHENMIN" => builder.lh_min = value.parse::<u8>().unwrap(),
                    "RIGHTHYPHENMIN" => builder.rh_min = value.parse::<u8>().unwrap(),
                    "COMPOUNDLEFTHYPHENMIN" => builder.clh_min = value.parse::<u8>().unwrap(),
                    "COMPOUNDRIGHTHYPHENMIN" => builder.crh_min = value.parse::<u8>().unwrap(),
                    "NOHYPHEN" => builder.nohyphen = Some(trimmed.to_string()),
                    _ => warn!("unknown keyword: {}", trimmed),
                }
                continue;
            }
            // Start a new hyphenation level?
            if trimmed == "NEXTLEVEL" {
                builders.push(LevelBuilder::new(bump));
                builder = builders.last_mut().unwrap();
                continue;
            }
            warn!("unknown keyword: {}", trimmed);
            continue;
        }
        // Patterns should always be provided in lowercase; complain if not, and discard
        // the bad pattern.
        if trimmed.chars().any(|c| c.is_uppercase()) {
            warn!("pattern \"{}\" not lowercased at line {}", trimmed, index);
            continue;
        }
        builder.add_pattern(trimmed);
    }

    // Create default first (compound-word) level if only one level was provided.
    // (Maybe this should be optional? Currently just copying libhyphen behavior.)
    if builders.len() == 1 {
        let (lh_min, rh_min, clh_min, crh_min) = (
            builders[0].lh_min,
            builders[0].rh_min,
            builders[0].clh_min,
            builders[0].crh_min,
        );
        builders.insert(0, LevelBuilder::new(bump));
        builder = builders.first_mut().unwrap();
        builder.add_pattern("1-1");
        builder.add_pattern("1'1");
        builder.add_pattern("1\u{2013}1"); // en-dash
        builder.add_pattern("1\u{2019}1"); // curly apostrophe
        builder.nohyphen = Some("',\u{2013},\u{2019},-".to_string());
        builder.lh_min = lh_min;
        builder.rh_min = rh_min;
        builder.clh_min = if clh_min > 0 {
            clh_min
        } else if lh_min > 0 {
            lh_min
        } else {
            3
        };
        builder.crh_min = if crh_min > 0 {
            crh_min
        } else if rh_min > 0 {
            rh_min
        } else {
            3
        };
    }

    // Put in fallback states in each builder.
    for builder in &mut builders {
        for (key, state_index) in builder.str_to_state.iter() {
            if key.is_empty() {
                continue;
            }
            let mut fallback_key = &key[..];
            while !fallback_key.is_empty() {
                fallback_key = &fallback_key[1..];
                if builder.str_to_state.contains_key(fallback_key) {
                    break;
                }
            }
            builder.states[*state_index as usize].fallback_state =
                builder.str_to_state[fallback_key];
        }
    }

    if compress {
        // Merge duplicate states to reduce size.
        for builder in &mut builders {
            builder.merge_duplicate_states();
        }
    }

    Ok(builders)
}

/// Write out the state machines representing a set of hyphenation rules
/// to the given output stream.
fn write_hyf_file<T: Write>(hyf_file: &mut T, levels: Vec<LevelBuilder>) -> std::io::Result<()> {
    if levels.is_empty() {
        return Err(Error::from(ErrorKind::InvalidData));
    }
    let mut flattened = vec![];
    for level in levels {
        flattened.push(level.flatten());
    }
    // Write file header: magic number, count of levels.
    hyf_file.write_all(b"Hyf0")?;
    let level_count: u32 = flattened.len() as u32;
    hyf_file.write_all(&level_count.to_le_bytes())?;
    // Write array of offsets to each level. First level will begin immediately
    // after the array of offsets.
    let mut offset: u32 = super::FILE_HEADER_SIZE as u32 + 4 * level_count;
    for flat in &flattened {
        hyf_file.write_all(&offset.to_le_bytes())?;
        offset += flat.len() as u32;
    }
    // Write the flattened data for each level.
    for flat in &flattened {
        hyf_file.write_all(flat)?;
    }
    Ok(())
}

/// The public API to the compilation process: reads `dic_file` and writes compiled tables
/// to `hyf_file`. The `compress` param determines whether extra processing to reduce the
/// size of the output is performed.
pub fn compile<T1: Read, T2: Write>(
    dic_file: T1,
    hyf_file: &mut T2,
    compress: bool,
) -> std::io::Result<()> {
    let bump = Bump::new();
    match read_dic_file(dic_file, &bump, compress) {
        Ok(dic) => write_hyf_file(hyf_file, dic),
        Err(e) => {
            warn!("parse error: {}", e);
            Err(Error::from(ErrorKind::InvalidData))
        }
    }
}
