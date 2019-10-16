extern crate mmhyph;

use mmhyph::Hyphenator;

fn main() {
    let dic_path = "hyph_en_US.hyf";

    let dic = match mmhyph::load_file(dic_path) {
        Some(dic) => dic,
        _ => panic!("failed to load dictionary {}", dic_path),
    };

    println!("{}", dic.hyphenate_word("haha", '-'));
    println!("{}", dic.hyphenate_word("hahaha", '-'));
    println!("{}", dic.hyphenate_word("photo", '-'));
    println!("{}", dic.hyphenate_word("photograph", '-'));
    println!("{}", dic.hyphenate_word("photographer", '-'));
    println!("{}", dic.hyphenate_word("photographic", '-'));
    println!("{}", dic.hyphenate_word("photographical", '-'));
    println!("{}", dic.hyphenate_word("photographically", '-'));
    println!("{}", dic.hyphenate_word("supercalifragilisticexpialidocious", '-'));
    println!("{}", dic.hyphenate_word("o'dwyer", '='));
    println!("{}", dic.hyphenate_word("o'callahan", '='));
    println!("{}", dic.hyphenate_word("o’dwyer", '='));
    println!("{}", dic.hyphenate_word("o’callahan", '='));
    println!("{}", dic.hyphenate_word("petti-fogging", '='));
    println!("{}", dic.hyphenate_word("e-mailing", '='));
    println!("{}", dic.hyphenate_word("-x-mailing", '='));
    println!("{}", dic.hyphenate_word("-strikeout-", '='));

    let dic2 = match mmhyph::load_file("tests/compound.hyf") {
        Some(dic) => dic,
        _ => panic!("failed to load dictionary {}", "tests/compound.hyf"),
    };

    println!("{}", dic2.hyphenate_word("motorcycle", '='));

}
