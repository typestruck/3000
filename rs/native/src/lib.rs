extern crate neon;
extern crate strum;
#[macro_use]
extern crate strum_macros;
extern crate rand;
extern crate num_traits;
extern crate num_derive;

use neon::prelude::*;
use rand::Rng;
mod data;
mod types;
use data::populate_grammatical_classes;
use types::*;
use num_traits::FromPrimitive;

fn generate(mut cx: FunctionContext) -> JsResult<JsString> {
        let what = cx.argument::<JsNumber>(0)?.value();
        let chars = cx.argument::<JsNumber>(1)?.value() as u8;
        let result = match FromPrimitive::from_f64(what) {
                Some(What::Name) => name(chars),
                Some(What::Description) => name(chars),       
                None => panic!("pattern matching failed for generate")
        };

        Ok(cx.string(result))
}

register_module!(mut cx, {
        cx.export_function("generate", generate)
});

fn name(total_chars: u8) -> String {
        if should_happen(60) {
                return simple_name(total_chars);
        }

        return simple_name(total_chars);
}

/// A "simple name" is a string containing <adjective> [, <adjective>] <noun>
fn simple_name(mut remaining_chars: u8) -> String {
        let grammatical_classes = populate_grammatical_classes();
        let mut names: Vec<String> = Vec::new();

        //this is kind of slow
        for class in filter_by_chance(vec![
                (GrammaticalClass::Adjectives, 100),
                (GrammaticalClass::Adjectives, 40),
                (GrammaticalClass::Nouns, 100),
        ]) {
                let keys: Vec<u8> = grammatical_classes[&class]
                        .keys()
                        .filter(|&k| *k <= remaining_chars - 1)
                        .cloned()
                        .collect();

                if keys.len() > 0 {
                        let words = &grammatical_classes[&class]
                                [&keys[rand::thread_rng().gen_range(0, keys.len())]];
                        let word = &words[rand::thread_rng().gen_range(0, words.len())];

                        names.push(word.to_string());
                        remaining_chars -= word.len() as u8;
                }
        }

        if names.len() == 3 {
                return format!("{}, {} {}", names[0], names[1], names[2]);
        }
        return names.join(" ");
}

/// Given a list, returns values according to their chance of occuring.
fn filter_by_chance(classes: Vec<(GrammaticalClass, u8)>) -> Vec<GrammaticalClass> {
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
                        filter_by_chance(vec![(GrammaticalClass::Adjectives, 100u8)]),
                        vec![GrammaticalClass::Adjectives]
                );
                assert_eq!(
                        filter_by_chance(vec![(GrammaticalClass::Adjectives, 0u8)]),
                        vec![]
                );
        }

        //for debugging purposes
        // #[test]
        // fn simple_name_test() {
        //         populate_grammatical_classes();
        // }
}
