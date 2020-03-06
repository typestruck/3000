
// //...
// thread_local!(static GRAMMATICAL_CLASS : RefCell<HashMap<GrammaticalClass, HashMap<u8, Vec<String>>>> = RefCell::new(populate_grammatical_classes()));

// fn populate_grammatical_classes() -> HashMap<GrammaticalClass, HashMap<u8, Vec<String>>> {
//         let mut grammatical_classes = HashMap::new();

//         for class in [
//             GrammaticalClass::Adjectives,
//             GrammaticalClass::Nouns,
//             GrammaticalClass::PluralNouns,
//             GrammaticalClass::Verbs,
//             GrammaticalClass::Adverbs,
//             GrammaticalClass::Other,
//         ]
//             .iter()
//         {
//             let class_name = class.to_string().to_lowercase();
//             let mut class_by_size = HashMap::new();

//             for word in fs::read_to_string(format!("../../data/{}", &class_name))
//                 .expect("Unable to read file")
//                 .split('\n')
//             {
//                 let vector = class_by_size.entry(word.len() as u8).or_insert(Vec::new());
//                 //...?
//                 vector.push(String::from(word));
//             }

//             grammatical_classes.insert(class.clone(), class_by_size);
//         }

//         for (key, value) in &grammatical_classes {
//             println!(",{} => map! {{", key);
//             for (k, v) in value {
//                 println!("{}u8 => vec!{:?},", k, v);
//             }
//             println!("}}");
//         }

//         grammatical_classes
// }
