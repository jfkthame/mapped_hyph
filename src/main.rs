/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

extern crate mapped_hyph;

use mapped_hyph::Hyphenator;

fn main() {
    let dic_path = "hyph_en_US.hyf";

    let dic = match mapped_hyph::load_file(dic_path) {
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

    let dic2 = match mapped_hyph::load_file("tests/compound.hyf") {
        Some(dic) => dic,
        _ => panic!("failed to load dictionary {}", "tests/compound.hyf"),
    };

    println!("{}", dic2.hyphenate_word("motorcycle", '='));

    let dic3 = match mapped_hyph::load_file("tests/rhmin.hyf") {
        Some(dic) => dic,
        _ => panic!("failed to load dictionary {}", dic_path),
    };
    println!("{}", dic3.hyphenate_word("övéit", '='));
    println!("{}", dic3.hyphenate_word("అంగడిధర", '='));

    let dic4 = match mapped_hyph::load_file("tests/num.hyf") {
        Some(dic) => dic,
        _ => panic!("failed to load dictionary {}", "tests/num.hyf"),
    };

    println!("{}", dic4.hyphenate_word("123foobar123", '='));
    println!("{}", dic4.hyphenate_word("123foobarfoobar", '='));
    println!("{}", dic4.hyphenate_word("foobarfoobar123", '='));
    println!("{}", dic4.hyphenate_word("123foobarfoobar123", '='));
}
