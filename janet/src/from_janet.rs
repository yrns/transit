use std::collections::HashMap;
use std::hash::Hash;
use std::path::PathBuf;

use janetrs::*;

// TODO: replace this w/ reflect? or something from bevy_mod_scripting?

pub trait FromJanet
where
    Self: Sized,
{
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError>;
}

// impl<T> FromJanet for T
// where
//     T: TryFrom<Janet>,
// {
//     fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
//         T::try_from(janet)
//     }
// }

// TODO: macro
impl<A, B, C> FromJanet for (A, B, C)
where
    A: FromJanet,
    B: FromJanet,
    C: FromJanet,
{
    // What about JanetArray?
    fn from_janet(value: Janet) -> Result<Self, JanetConversionError> {
        match value.unwrap() {
            TaggedJanet::Tuple(tuple) => {
                // Check the length. In case its longer?
                if tuple.len() != 3 {
                    // TODO A more comprehensive error type.
                    return Err(JanetConversionError::Other);
                }
                match (tuple.get(0), tuple.get(1), tuple.get(2)) {
                    (Some(a), Some(b), Some(c)) => {
                        Ok((A::from_janet(*a)?, B::from_janet(*b)?, C::from_janet(*c)?))
                    }
                    _ => Err(JanetConversionError::Other),
                }
            }
            _ => Err(JanetConversionError::WrongKind(
                JanetType::Tuple,
                value.kind(),
            )),
        }
    }
}

impl<K, V> FromJanet for HashMap<K, V>
where
    K: FromJanet + Eq + Hash,
    V: FromJanet,
{
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        JanetTable::try_from(janet).and_then(|t| {
            t.iter()
                .try_fold(Self::with_capacity(t.len() as usize), |mut acc, (k, v)| {
                    let k = K::from_janet(*k)?;
                    let v = V::from_janet(*v)?;
                    acc.insert(k, v);
                    Ok(acc)
                })
        })
    }
}

// We're really only using this to convert symbols, which are (Janet) symbols. String is too generic.
impl FromJanet for String {
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        match janet.unwrap() {
            TaggedJanet::Symbol(s) => std::str::from_utf8(s.as_bytes())
                // .to_str() JanetString has this
                .map_err(|_| JanetConversionError::Other)
                .map(String::from),
            _ => Err(JanetConversionError::WrongKind(
                JanetType::String,
                janet.kind(),
            )),
        }
    }
}

impl FromJanet for PathBuf {
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        match janet.unwrap() {
            TaggedJanet::String(s) => s
                .to_path()
                .map_err(|_| JanetConversionError::Other)
                .map(PathBuf::from),
            _ => Err(JanetConversionError::WrongKind(
                JanetType::String,
                janet.kind(),
            )),
        }
    }
}

impl<T> FromJanet for Option<T>
where
    T: FromJanet,
{
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        if janet.is_nil() {
            Ok(None)
        } else {
            T::from_janet(janet).map(|v| Some(v))
        }
    }
}

impl FromJanet for usize {
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        f64::try_from(janet).map(|f| f as usize)
    }
}
