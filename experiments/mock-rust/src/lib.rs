extern crate strum; 
#[macro_use]
extern crate strum_macros; 
extern crate rand;

pub mod bender {
    use std::collections::HashMap;
    use std::string::ToString;
    use rand::Rng;
    use std::fs;
    use std::cell::RefCell;

    #[derive(Display, Debug)]
    enum GrammaticalClass {
        Adjectives,
        Nouns,
        PluralNouns,
        Verbs,
        Adverbs,
        Other 
    }

    //...
    thread_local!(static GRAMMATICAL_CLASS : RefCell<HashMap<GrammaticalClass, HashMap<u8, Vec<String>>>> = RefCell::new(populate_grammatical_classes()));      
    
    fn populate_grammatical_classes() -> HashMap<GrammaticalClass, HashMap<u8, Vec<String>>> {
        let mut grammatical_classes = HashMap::new();

        for class in [GrammaticalClass::Adjectives, GrammaticalClass::Nouns, GrammaticalClass::PluralNouns, GrammaticalClass::Verbs, GrammaticalClass::Adverbs, GrammaticalClass::Other].iter() {
            let class_name = class.to_string();
            let mut class_by_size = HashMap::new();
    
            for word in fs::read_to_string(&class_name).expect("Unable to read file").split('\n'){
                let vector = class_by_size.entry(word.len() as u8).or_insert(Vec::new());
                //...?
                vector.push(String::from(word));
            }

            grammatical_classes.insert(class, class_by_size);
        }      

        grammatical_classes
    }

    /// A "simple name" is a string containing <adjective> [, <adjective>] <noun>
    pub fn simple_name() {
        //for mocking purposes
        let mut name = String::new(),
            mut remaining_chars = rand::thread_rng().gen_range(10, 31);

        GRAMMATICAL_CLASS.with(|g_c| {
            //this is kind of slow
            for class in filterByChance((vec![(GrammaticalClass::Adjectives, 100), (GrammaticalClass::Adjectives, 40), (GrammaticalClass::Nouns, 100)]).iter()){
                //this needs to be an array of keys .....
                let entries = g_c.entry(GrammaticalClass::Adjectives).iter().filter(|&(k , _)| *k <= remaining_chars - 1);

                if entries.len() > 0 {
                  let words = entries.entry(rand::thread_rng().gen_range(0, entries.len()));


                    name.add(words[])
                }
            }
        });
    }

    fn filterByChance<I  : Iterator>(classes : I<(GrammaticalClass, u8)> ) -> I<(GrammaticalClass, u8)> {
        classes.filter(|(class, chance )| shouldHappen(chance)).map(|(class, _ )| class)
    }

    fn shouldHappen(chance : u8) -> bool {
        rand::thread_rng().gen_range(1, 100) <= chance
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
