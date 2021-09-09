#[macro_use]
extern crate strum_macros;
extern crate either;
extern crate num_derive;
extern crate num_traits;
extern crate rand;
#[macro_use]
extern crate lazy_static;

mod data;
mod name;
mod description;
mod types;

use neon::prelude::*;
use name::*;
use description::*;
use types::*;
use num_traits::FromPrimitive;

fn generate(mut cx: FunctionContext) -> JsResult<JsString> {
        let what = cx.argument::<JsNumber>(0)?.value(&mut cx);
        let chars = cx.argument::<JsNumber>(0)?.value(&mut cx) as u16;
        let result = match FromPrimitive::from_f64(what) {
                Some(What::Name) => generate_name(chars as u8),
                Some(What::Description) => generate_description(chars),
                None => panic!("pattern matching failed for generate"),
        };

        return Ok(cx.string(capitalize(result)));
}

fn capitalize(text: String) -> String {
        let mut letters = text.chars();

        match letters.next() {
                None => String::new(),
                Some(l) => l.to_uppercase().chain(letters).collect(),
        }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("generate", generate)?;
    Ok(())
}

#[cfg(test)]
mod tests {
        use super::*;

        #[test]
        fn capitalize_test() {
                assert_eq!("Test", capitalize("test".to_string()));
                assert_eq!("Test", capitalize("Test".to_string()));
                assert_eq!("TEST", capitalize("TEST".to_string()));
        }
}
