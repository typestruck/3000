use num_derive::FromPrimitive;    

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
