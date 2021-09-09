use num_derive::FromPrimitive;
use strum_macros::{Display};

#[derive(Display, Debug, PartialEq, Eq, Hash, Clone)]
pub enum GrammaticalClass {
        Adjectives,
        Nouns,
        #[strum(serialize = "plural-nouns")]
        PluralNouns,
        Verbs,
        Adverbs,
        Other,
}

#[derive(FromPrimitive)]
pub enum What {
        Name,
        Description
}
