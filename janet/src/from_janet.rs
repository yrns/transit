use std::collections::HashMap;
use std::hash::Hash;

use janetrs::*;

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
    fn from_janet(value: Janet) -> Result<Self, JanetConversionError> {
        if let TaggedJanet::Tuple(tuple) = value.unwrap() {
            match (tuple.get(0), tuple.get(1), tuple.get(2)) {
                (Some(a), Some(b), Some(c)) => {
                    match (A::from_janet(*a), B::from_janet(*b), C::from_janet(*c)) {
                        (Ok(a), Ok(b), Ok(c)) => Ok((a, b, c)),
                        _ => Err(JanetConversionError),
                    }
                }
                _ => Err(JanetConversionError),
            }
        } else {
            Err(JanetConversionError)
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

impl FromJanet for String {
    fn from_janet(janet: Janet) -> Result<Self, JanetConversionError> {
        match janet.unwrap() {
            TaggedJanet::Keyword(k) => std::str::from_utf8(k.as_bytes())
                .map(String::from)
                .map_err(|_| JanetConversionError),
            TaggedJanet::Symbol(s) => std::str::from_utf8(s.as_bytes())
                .map(String::from)
                .map_err(|_| JanetConversionError),
            TaggedJanet::String(s) => s
                .to_str()
                .map_err(|_| JanetConversionError)
                .map(|s| s.into()),
            _ => Err(JanetConversionError),
        }
    }
}

// impl FromJanet for PathBuf?

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
