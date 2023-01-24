use janetrs::{
    client::{Error, JanetClient},
    Janet, JanetString, TaggedJanet,
};
use std::{collections::HashMap, path::Path};

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
        Ok(convert(res))
    } else {
        Ok(None)
    }
}

pub fn convert(symbols: Janet) -> Option<SymbolMap> {
    match symbols.unwrap() {
        TaggedJanet::Table(table) => {
            let mut res: SymbolMap = HashMap::new();
            for (k, v) in table {
                match (k.unwrap(), v.unwrap()) {
                    (TaggedJanet::Symbol(k), TaggedJanet::Tuple(v)) => {
                        match (
                            v.get(0).unwrap().unwrap(),
                            v.get(1).unwrap().unwrap(),
                            v.get(2).unwrap().unwrap(),
                        ) {
                            (
                                TaggedJanet::String(path),
                                TaggedJanet::Number(line),
                                TaggedJanet::Number(column),
                            ) => {
                                res.insert(
                                    JanetString::new(k).to_str_lossy().into(),
                                    (path.to_str_lossy().into(), line as usize, column as usize),
                                );
                            }
                            _ => {
                                println!("bad value in symbol map: {:?}", v);
                            }
                        }
                    }
                    _ => {
                        println!("bad value in symbol map: {:?}: {:?}", k, v);
                    }
                }
            }
            Some(res)
        }
        _ => None,
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
                Path::new("./test/symbols.janet")
                    .canonicalize()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
                1,
                1
            )
        );
    }
}
