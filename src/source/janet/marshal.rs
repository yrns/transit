use janetrs::{lowlevel, *};

pub fn serialize<S>(janet: &Janet, s: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut buf = JanetBuffer::new();
    let mut rrev = JanetTable::new(); // Why mut?
    let flags = lowlevel::JANET_MARSHAL_NO_CYCLES;
    //let flags = flags & lowlevel::JANET_MARSHAL_UNSAFE;

    unsafe {
        // This can panic and jcatch! requires a fiber...
        lowlevel::janet_marshal(
            buf.as_mut_raw(),
            janet.raw_data(),
            rrev.as_mut_raw(),
            flags as i32,
        )
        //.map_err(serde::ser::Error::custom)?;
    }

    s.serialize_bytes(buf.as_bytes())
}

pub fn deserialize<'de, D>(d: D) -> Result<Janet, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct BytesVisitor;

    impl<'de> serde::de::Visitor<'de> for BytesVisitor {
        type Value = Janet;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("bytes")
        }

        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            let mut reg = JanetTable::new();

            let j = unsafe {
                // This can panic and jcatch! requires a fiber...
                lowlevel::janet_unmarshal(
                    v.as_ptr(),
                    v.len(),
                    0,
                    reg.as_mut_raw(),
                    std::ptr::null::<u8> as *mut *const u8,
                )
                //.map_err(E::custom)
            };

            Ok(Janet::from(j))
        }
    }

    d.deserialize_bytes(BytesVisitor)
}
