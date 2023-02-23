use janetrs::{lowlevel, *};

pub fn serialize<S>(janet: &Janet, s: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut buf = JanetBuffer::new();
    let flags = lowlevel::JANET_PRETTY_ONELINE & lowlevel::JANET_PRETTY_NOTRUNC;

    unsafe {
        lowlevel::janet_pretty(buf.as_mut_raw(), 48, flags as i32, janet.raw_data());
    }

    s.serialize_str(buf.to_str().map_err(serde::ser::Error::custom)?)
}

pub fn deserialize<'de, D>(d: D) -> Result<Janet, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct StringVisitor;

    impl<'de> serde::de::Visitor<'de> for StringVisitor {
        type Value = Janet;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("string")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            // Eval later?
            Ok(JanetString::from(v).into())
        }
    }

    d.deserialize_str(StringVisitor)
}
