use data::MARKOV_CHAIN;
use rand::Rng;

/// Descriptions are markov chains
pub fn generate_description(mut remaining_chars: u16) -> String {
        let description = &mut Vec::new();
        let keys: Vec<&(&str, &str)> = MARKOV_CHAIN.keys().collect();
        let mut prefix = keys[rand::thread_rng().gen_range(0, keys.len())].clone();

        loop {
                if let Some(words) = MARKOV_CHAIN.get(&prefix) {
                        let fitting_words: Vec<&str> = words
                                .iter()
                                .filter(|w| (w.len() as u16) < remaining_chars - 1)
                                .cloned()
                                .collect();
                        let fitting = fitting_words.len();

                        if fitting > 0 {
                                let wd = fitting_words[rand::thread_rng().gen_range(0, fitting)];

                                description.push(wd.to_owned());
                                remaining_chars -= wd.len() as u16 + 1;

                                prefix = (prefix.1, wd);
                                continue;
                        }
                }

                end_sentence(description);
                break
        }

        return description.join(" ");
}

/// Half assed attempt to terminate a sentence with a punctuation mark
fn end_sentence(description: &mut Vec<String>) {
        if let Some(word) = description.last_mut() {
                let punctuation = [".", "?", "!", ";", "--", "-", ":", ","];

                if punctuation.iter().all(|p| !word.ends_with(p)) {
                        word.push_str(".");
                }
        }
}
