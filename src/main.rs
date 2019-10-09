use std::fs::File;
use memmap::Mmap;

use hyphenate;

fn main() {
    let file = match File::open("hyph_en_US.hyf") {
        Err(why) => panic!("couldn't open file: {}", why),
        Ok(file) => file,
    };

    let mmap = match unsafe { Mmap::map(&file) } {
        Err(why) => panic!("couldn't mmap file: {}", why),
        Ok(mmap) => mmap,
    };

    let hyph = hyphenate::load_hyphenation(&mmap);

    #[cfg(debug_assertions)] // print results of a few examples
    {
        println!("{}", hyph.hyphenate_word("haha", '-'));
        println!("{}", hyph.hyphenate_word("hahaha", '-'));
        println!("{}", hyph.hyphenate_word("photo", '-'));
        println!("{}", hyph.hyphenate_word("photograph", '-'));
        println!("{}", hyph.hyphenate_word("photographer", '-'));
        println!("{}", hyph.hyphenate_word("photographic", '-'));
        println!("{}", hyph.hyphenate_word("photographical", '-'));
        println!("{}", hyph.hyphenate_word("photographically", '-'));
        println!("{}", hyph.hyphenate_word("supercalifragilisticexpialidocious", '-'));
    }

    #[cfg(not(debug_assertions))] // run over a large list of words
    {
        let words_file = match File::open("/usr/share/dict/words") {
            Err(why) => panic!("couldn't open file: {}", why),
            Ok(file) => file,
        };
        use std::io::{BufRead,BufReader};
        let reader = BufReader::new(words_file);
        for (_index, line) in reader.lines().enumerate() {
            let line = line.unwrap(); // Ignore errors.
            let result = hyph.hyphenate_word(&line, '=');
            println!("{}", result);
        }
    }
}
