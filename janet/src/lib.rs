mod from_janet;

use from_janet::FromJanet;
use janetrs::{
    client::{Error, JanetClient},
    Janet, JanetSymbol, TaggedJanet,
};
use std::{collections::HashMap, path::Path};

pub struct JanetContext {
    //client: &'a JanetClient,
    pub context: Janet,
}

#[derive(Clone, Debug)]
pub struct State {
    pub enter: Janet,
    pub exit: Janet,
    pub local: Janet,
}

// Can this be nil instead? This is only used for the root state. Is enter/exit even interesting for
// root states?
impl Default for State {
    fn default() -> Self {
        Self {
            enter: Janet::nil(),
            exit: Janet::nil(),
            local: Janet::nil(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Transition {
    pub guard: Janet,
    pub local: Janet,
}

impl transit::Context for JanetContext {
    type Event = Janet;
    type State = State;
    type Transition = Transition;

    fn dispatch(&mut self, event: &Janet) {
        println!("dispatch: event: {:?}", event);
    }

    fn transition(&mut self, source: &State, target: &State) {
        println!("transition: source: {:?} target: {:?}", source, target);
    }
}

// Resolve a symbol and return the result if it's a function.
pub fn resolve<'a>(symbol: impl Into<JanetSymbol<'a>>, client: &JanetClient) -> Janet {
    client
        .env()
        .and_then(|env| env.resolve(symbol))
        .and_then(|value| match value.unwrap() {
            TaggedJanet::Function(_) => Some(value),
            _ => None,
        })
        .unwrap_or(Janet::nil())
}

impl transit::State<JanetContext> for State {
    fn enter(&mut self, ctx: &mut JanetContext, event: Option<&Janet>) {
        if let TaggedJanet::Function(mut f) = self.enter.unwrap() {
            match f.call(&[self.local, ctx.context, *event.unwrap_or(&Janet::nil())]) {
                Ok(_) => (),
                Err(err) => println!("err in enter: {:?}", err),
            }
        }
    }

    fn exit(&mut self, ctx: &mut JanetContext, event: Option<&Janet>) {
        if let TaggedJanet::Function(mut f) = self.exit.unwrap() {
            match f.call(&[self.local, ctx.context, *event.unwrap_or(&Janet::nil())]) {
                Ok(_) => (),
                Err(err) => println!("err in enter: {:?}", err),
            }
        }
    }
}

impl transit::Transition<JanetContext> for Transition {
    fn guard(&mut self, ctx: &mut JanetContext, event: &Janet) -> bool {
        if let TaggedJanet::Function(mut f) = self.guard.unwrap() {
            match f.call(&[self.local, ctx.context, *event]) {
                Ok(res) => {
                    println!("guard result: {:?}", res);
                    !res.is_nil()
                }
                Err(err) => {
                    println!("err in guard: {:?}", err);
                    false
                }
            }
        } else {
            println!("no guard");
            false
        }
    }
}

pub type SymbolMap = HashMap<String, (String, usize, usize)>;

pub fn get_symbols(path: impl AsRef<Path>) -> Result<Option<SymbolMap>, Error> {
    let client = JanetClient::init_with_default_env()?;

    // Set the system path to the specified path's directory and drop the extension from the file
    // name. Janet interprets a leading "/" as relative, full paths don't work.
    let path = path.as_ref();
    if let (Some(parent), Some(stem)) = (path.parent(), path.file_stem()) {
        // Requiring a module returns a table w/ symbols. TODO escape? use parser instead?
        let query = format!(
            r#"(setdyn :syspath "{}") (tabseq [[k v] :pairs (require "{}")] k (get v :source-map))"#,
            parent.display(),
            stem.to_string_lossy(),
        );

        //dbg!(&query);

        let res = client.run(query)?;

        Ok(SymbolMap::from_janet(res)
            .map_err(|e| {
                println!("error in SymbolMap::from_janet: {:?}", e);
                e
            })
            .ok())
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        assert_eq!(
            get_symbols("test/symbols")
                .unwrap()
                .unwrap()
                .remove("a-symbol")
                .unwrap(),
            (
                // If the specified path is relative, the returned paths will be, too.
                "test/symbols.janet".to_owned(),
                1,
                1
            )
        );
    }
}
