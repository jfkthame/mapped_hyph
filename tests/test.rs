#![feature(test)]

extern crate test;
extern crate mmhyph;

#[cfg(test)]
mod tests {
    use mmhyph::Hyphenator;

    #[test]
    fn basic_tests() {
        let dic_path = "hyph_en_US.hyf";
        let hyph = match mmhyph::load_file(dic_path) {
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

    #[test]
    fn base() {
        let dic_path = "tests/base.hyf";
        let dic = match mmhyph::load_file(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        use std::fs::File;
        use std::io::{BufRead,BufReader};
        let words: Vec<String> = {
            let file = File::open("tests/base.word").unwrap();
            BufReader::new(file).lines().map(|l| l.unwrap()).collect()
        };
        let hyphs: Vec<String> = {
            let file = File::open("tests/base.hyph").unwrap();
            BufReader::new(file).lines().map(|l| l.unwrap()).collect()
        };
        for i in 0 .. words.len() {
            assert_eq!(dic.hyphenate_word(&words[i], '='), hyphs[i]);
        }
    }

    #[test]
    fn compound() {
        let dic_path = "tests/compound.hyf";
        let dic = match mmhyph::load_file(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        assert_eq!(dic.hyphenate_word("motorcycle", '-'), "mo-tor-cy-cle");
    }

    #[test]
    fn compound4() {
        let dic_path = "tests/compound4.hyf";
        let dic = match mmhyph::load_file(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        assert_eq!(dic.hyphenate_word("motorcycle", '-'), "motor-cycle");
    }

    #[test]
    fn compound5() {
        let dic_path = "tests/compound5.hyf";
        let dic = match mmhyph::load_file(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        assert_eq!(dic.hyphenate_word("postea", '-'), "post-e-a");
    }

    #[test]
    fn compound6() {
        let dic_path = "tests/compound6.hyf";
        let dic = match mmhyph::load_file(dic_path) {
            Some(dic) => dic,
            _ => panic!("failed to load dictionary {}", dic_path),
        };
        assert_eq!(dic.hyphenate_word("meaque", '-'), "me-a-que");
    }
}
