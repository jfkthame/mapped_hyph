#![feature(test)]
extern crate test;
extern crate mmhyph;

#[cfg(test)]
mod tests {
    #[test]
    fn basic_tests() {
        let dic_path = "hyph_en_US.hyf";
        let hyph = match mmhyph::load(dic_path) {
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
}
