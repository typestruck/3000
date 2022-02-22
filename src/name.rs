use either::Either;
use either::Either::*;
use rand::Rng;
use crate::types::*;
use crate::data::GRAMATICAL_CLASSES;

/// Names are generated according to the following patterns:
///  <adjective> [, <adjective>] <noun>
///  [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
pub fn generate_name(total_chars: u8) -> String {
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
        ];
        return make_names(remaining_chars, classes_chances).join(" ");
}

fn make_names(
        mut remaining_chars: u8,
        classes_chances: Vec<(Either<&str, GrammaticalClass>, u8)>,
) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();

        for class in filter_by_chance(classes_chances) {
                match class {
                        Right(cl) => {
                                let word = get_word(cl, remaining_chars);

                                if let Some(wd) = word {
                                        remaining_chars -= wd.len() as u8;
                                        names.push(wd);
                                }
                        }
                        Left(word) => {
                                let size = word.len() as u8;

                                if remaining_chars - 1 >= size {
                                        names.push(word.to_string());
                                        remaining_chars -= size;
                                }
                        }
                }
        }

        return names;
}

/// Pick a grammatical class random word up to the given size
fn get_word(
        class: GrammaticalClass,
        remaining_chars: u8,
) -> Option<String> {
        let keys: Vec<u8> = GRAMATICAL_CLASSES[&class]
                .keys()
                .filter(|&k| *k <= remaining_chars - 1)
                .cloned()
                .collect();

        if keys.len() > 0 {
                let words = &GRAMATICAL_CLASSES[&class]
                        [&keys[rand::thread_rng().gen_range(0, keys.len())]];
                let word = &words[rand::thread_rng().gen_range(0, words.len())];

                return Some(word.to_string());
        }

        return None;
}

/// Given a list, returns values according to their chance of occurring
fn filter_by_chance(
        classes: Vec<(Either<&str, GrammaticalClass>, u8)>,
) -> Vec<Either<&str, GrammaticalClass>> {
        return classes
                .iter()
                .filter(|(_, chance)| should_happen(*chance))
                .map(|(class, _)| class)
                .cloned()
                .collect();
}

/// Chance of an event occurring out of 100 times
fn should_happen(chance: u8) -> bool {
        return rand::thread_rng().gen_range(1, 100) <= chance;
}

#[cfg(test)]
mod tests {
        use super::*;

        #[test]
        fn should_happen_test() {
                assert_eq!(true, should_happen(100));
                assert_eq!(false, should_happen(0));
        }

        #[test]
        fn filter_by_chance_test() {
                assert_eq!(
                        vec![Right(GrammaticalClass::Adjectives)],
                        filter_by_chance(vec![(Right(GrammaticalClass::Adjectives), 100u8)])
                );
                assert!(
                        filter_by_chance(vec![(Right(GrammaticalClass::Adjectives), 0u8)])
                                .is_empty()
                );
        }
}
