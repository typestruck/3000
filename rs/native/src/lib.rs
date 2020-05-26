extern crate neon;
extern crate strum;
#[macro_use]
extern crate strum_macros;
extern crate either;
extern crate num_derive;
extern crate num_traits;
extern crate rand;
extern crate unicode_segmentation;

use neon::prelude::*;
use rand::Rng;
use std::collections::HashMap;
mod data;
mod types;
use data::populate_grammatical_classes;
use either::Either;
use either::Either::*;
use num_traits::FromPrimitive;
use types::*;
use unicode_segmentation::UnicodeSegmentation;

fn generate(mut cx: FunctionContext) -> JsResult<JsString> {
        let what = cx.argument::<JsNumber>(0)?.value();
        let chars = cx.argument::<JsNumber>(1)?.value() as u8;
        let result = match FromPrimitive::from_f64(what) {
                Some(What::Name) => name(chars),
                Some(What::Description) => name(chars),
                None => panic!("pattern matching failed for generate"),
        };

        return Ok(cx.string(capitalize(result)));
}

register_module!(mut cx, { cx.export_function("generate", generate) });

fn capitalize(text: String) -> String {
        let mut letters = text.chars();

        match letters.next() {
                None => String::new(),
                Some(l) => l.to_uppercase().chain(letters).collect(),
        }
}

/// Names are generated according to the following patterns:
///  <adjective> [, <adjective>] <noun>
///  [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
fn name(total_chars: u8) -> String {
        if should_happen(60) {
                return simple_name(total_chars);
        }

        return complex_name(total_chars);
}

/// A "simple name" is a string containing <adjective> [, <adjective>] <noun>
fn simple_name(remaining_chars: u8) -> String {
        let classes_chances = vec![
                (Right(GrammaticalClass::Adjectives), 100),
                (Right(GrammaticalClass::Adjectives), 40),
                (Right(GrammaticalClass::Nouns), 100),
        ];
        let names = make_names(remaining_chars, classes_chances);

        if names.len() == 3 {
                return format!("{}, {} {}", names[0], names[1], names[2]);
        }
        return names.join(" ");

}

/// A "complex name" is a string containing [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
fn complex_name(remaining_chars: u8) -> String {
        let classes_chances = vec![
                (Right(GrammaticalClass::Adjectives), 20),
                (Right(GrammaticalClass::Nouns), 100),
                (Left(if should_happen(70) { "who" } else { "that" }), 100),
                (Right(GrammaticalClass::Adverbs), 20),
                (Right(GrammaticalClass::Verbs), 100),
                (Right(GrammaticalClass::Adjectives), 10),
                (Right(GrammaticalClass::PluralNouns), 30),
                (Right(GrammaticalClass::Other), 5),
        ] ;
        return make_names(remaining_chars, classes_chances).join(" ");
}

fn make_names(mut remaining_chars: u8, classes_chances: Vec<(Either<&str, GrammaticalClass>, u8)>) -> Vec<String> {
        let grammatical_classes = populate_grammatical_classes();
        let mut names: Vec<String> = Vec::new();

        for class in filter_by_chance(classes_chances) {
                match class {
                        Right(cl) => {
                                let word = get_word(&grammatical_classes, cl, remaining_chars);

                                match word {
                                        Some(wd) => {
                                                remaining_chars -= wd.len() as u8;
                                                names.push(wd);
                                        }
                                        None => {}
                                }
                        }
                        Left(word) => {
                                let size = word.graphemes(true).count() as u8;

                                if remaining_chars - 1 >= size
                                {
                                        names.push(word.to_string());
                                        remaining_chars -= size;
                                }
                        }
                }
        }

        return names;
}

/// Pick a gramatical class random word up to the given size
fn get_word(
        grammatical_classes: &HashMap<GrammaticalClass, HashMap<u8, Vec<&'static str>>>,
        class: GrammaticalClass,
        remaining_chars: u8,
) -> Option<String> {
        let keys: Vec<u8> = grammatical_classes[&class]
                .keys()
                .filter(|&k| *k <= remaining_chars - 1)
                .cloned()
                .collect();

        if keys.len() > 0 {
                let words = &grammatical_classes[&class]
                        [&keys[rand::thread_rng().gen_range(0, keys.len())]];
                let word = &words[rand::thread_rng().gen_range(0, words.len())];

                return Some(word.to_string());
        }

        return None;
}

/// Given a list, returns values according to their chance of occuring
fn filter_by_chance(classes: Vec<(Either<&str, GrammaticalClass>, u8)>) -> Vec<Either<&str, GrammaticalClass>> {
        classes.iter()
                .filter(|(_, chance)| should_happen(*chance))
                .map(|(class, _)| class)
                .cloned()
                .collect()
}

/// Chance of an event occuring out of 100 times
fn should_happen(chance: u8) -> bool {
        rand::thread_rng().gen_range(1, 100) <= chance
}

#[cfg(test)]
mod tests {
        use super::*;

        #[test]
        fn should_happen_test() {
                assert_eq!(should_happen(100), true);
                assert_eq!(should_happen(0), false);
        }

        #[test]
        fn filter_by_chance_test() {
                assert_eq!(
                        filter_by_chance(vec![(Right(GrammaticalClass::Adjectives), 100u8)]),
                        vec![Right(GrammaticalClass::Adjectives)]
                );
                assert_eq!(
                        filter_by_chance(vec![(Right(GrammaticalClass::Adjectives), 0u8)]),
                        vec![]
                );
        }

        //for debugging purposes
        // #[test]
        // fn simple_name_test() {
        //         populate_grammatical_classes();
        // }
}
