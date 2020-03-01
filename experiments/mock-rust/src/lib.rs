extern crate strum;
#[macro_use]
extern crate strum_macros;
extern crate rand;

pub mod bender {
    use rand::Rng;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::fs;
    use std::string::ToString;

    #[no_mangle]
    fn generate_name() -> String {
        return simple_name(100);
    }

    #[derive(Display, Debug, PartialEq, Eq, Hash, Clone)]
    enum GrammaticalClass {
        Adjectives,
        Nouns,
        #[strum(serialize = "plural-nouns")]
        PluralNouns,
        Verbs,
        Adverbs,
        Other,
    }

    //...
    thread_local!(static GRAMMATICAL_CLASS : RefCell<HashMap<GrammaticalClass, HashMap<u8, Vec<String>>>> = RefCell::new(populate_grammatical_classes()));

    fn populate_grammatical_classes() -> HashMap<GrammaticalClass, HashMap<u8, Vec<String>>> {
        let mut grammatical_classes = HashMap::new();

        for class in [
            GrammaticalClass::Adjectives,
            GrammaticalClass::Nouns,
            GrammaticalClass::PluralNouns,
            GrammaticalClass::Verbs,
            GrammaticalClass::Adverbs,
            GrammaticalClass::Other,
        ]
            .iter()
        {
            let class_name = class.to_string().to_lowercase();
            let mut class_by_size = HashMap::new();

            for word in fs::read_to_string(format!("../../data/{}", &class_name))
                .expect("Unable to read file")
                .split('\n')
            {
                let vector = class_by_size.entry(word.len() as u8).or_insert(Vec::new());
                //...?
                vector.push(String::from(word));
            }

            grammatical_classes.insert(class.clone(), class_by_size);
        }

        grammatical_classes
    }

    /// A "simple name" is a string containing <adjective> [, <adjective>] <noun>
    pub fn simple_name(mut remaining_chars: u8) -> String {
        let mut names : Vec<String> = Vec::new();

        GRAMMATICAL_CLASS.with(|g_c| {
            let g_c = g_c.borrow();

            //this is kind of slow
            for class in filter_by_chance(vec![
                (GrammaticalClass::Adjectives, 100),
                (GrammaticalClass::Adjectives, 40),
                (GrammaticalClass::Nouns, 100),
            ]) {
                let keys: Vec<u8> = g_c[&class]
                    .keys()
                    .filter(|&k| *k <= remaining_chars - 1)
                    .cloned()
                    .collect();

                if keys.len() > 0 {
                    let words = &g_c[&class][&keys[rand::thread_rng().gen_range(0, keys.len())]];
                    let word = &words[rand::thread_rng().gen_range(0, words.len())];
                    
                    names.push(word.to_string());                    
                    remaining_chars -= word.len() as u8;
                }
            }
        });

        if names.len() == 3 {
            return format!("{}, {} {}", names[0], names[1], names[2]);
        }
        
        return names.join(" ");
    }

    /// Given a list, returns values according to their chance of occuring.
    fn filter_by_chance(classes: Vec<(GrammaticalClass, u8)>) -> Vec<GrammaticalClass> {
        classes
            .iter()
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
        #[test]
        fn simple_name_test() {
            let mut name = simple_name(rand::thread_rng().gen_range(10, 31));

            println!("{}", &name);
            assert_eq!(&name, &name);

            name = simple_name(rand::thread_rng().gen_range(0, 20));
            println!("{}", &name);
            assert_eq!(&name, &name);

            name = simple_name(rand::thread_rng().gen_range(20, 209));
            println!("{}", &name);
            assert_eq!(&name, &name);
        }
    }
}
