#![feature(prelude_import)]
#![no_std]
#[prelude_import]
use ::std::prelude::v1::*;
#[macro_use]
extern crate std as std;
use anyhow::Result;
use petgraph::{
    graph::{IndexType, NodeIndex},
    stable_graph::StableDiGraph,
};
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{Deserialize, Serialize};
use std::boxed::Box;
use typetag;
pub struct Statechart<E, Ix: IndexType> {
    pub id: String,
    pub initial: Initial<Ix>,
    pub graph: StableDiGraph<State<Ix>, Transition<E>, Ix>,
    pub active_state: Option<NodeIndex<Ix>>,
}
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_Statechart: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<E, Ix: IndexType> _serde::Serialize for Statechart<E, Ix>
    where
        E: _serde::Serialize,
        Ix: _serde::Serialize,
    {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = match _serde::Serializer::serialize_struct(
                __serializer,
                "Statechart",
                false as usize + 1 + 1 + 1 + 1,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "id", &self.id)
            {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "initial",
                &self.initial,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "graph",
                &self.graph,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "active_state",
                &self.active_state,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_Statechart: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de, E, Ix: IndexType> _serde::Deserialize<'de> for Statechart<E, Ix>
    where
        E: _serde::Deserialize<'de>,
        Ix: _serde::Deserialize<'de>,
    {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __field3,
                __ignore,
            }
            struct __FieldVisitor;
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => _serde::export::Ok(__Field::__field0),
                        1u64 => _serde::export::Ok(__Field::__field1),
                        2u64 => _serde::export::Ok(__Field::__field2),
                        3u64 => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 4",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "id" => _serde::export::Ok(__Field::__field0),
                        "initial" => _serde::export::Ok(__Field::__field1),
                        "graph" => _serde::export::Ok(__Field::__field2),
                        "active_state" => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(
                    self,
                    __value: &[u8],
                ) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"id" => _serde::export::Ok(__Field::__field0),
                        b"initial" => _serde::export::Ok(__Field::__field1),
                        b"graph" => _serde::export::Ok(__Field::__field2),
                        b"active_state" => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de, E, Ix: IndexType>
            where
                E: _serde::Deserialize<'de>,
                Ix: _serde::Deserialize<'de>,
            {
                marker: _serde::export::PhantomData<Statechart<E, Ix>>,
                lifetime: _serde::export::PhantomData<&'de ()>,
            }
            impl<'de, E, Ix: IndexType> _serde::de::Visitor<'de> for __Visitor<'de, E, Ix>
            where
                E: _serde::Deserialize<'de>,
                Ix: _serde::Deserialize<'de>,
            {
                type Value = Statechart<E, Ix>;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "struct Statechart")
                }
                #[inline]
                fn visit_seq<__A>(
                    self,
                    mut __seq: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match match _serde::de::SeqAccess::next_element::<String>(&mut __seq) {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        } {
                            _serde::export::Some(__value) => __value,
                            _serde::export::None => {
                                return _serde::export::Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct Statechart with 4 elements",
                                ));
                            }
                        };
                    let __field1 = match match _serde::de::SeqAccess::next_element::<Initial<Ix>>(
                        &mut __seq,
                    ) {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct Statechart with 4 elements",
                            ));
                        }
                    };
                    let __field2 = match match _serde::de::SeqAccess::next_element::<
                        StableDiGraph<State<Ix>, Transition<E>, Ix>,
                    >(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct Statechart with 4 elements",
                            ));
                        }
                    };
                    let __field3 = match match _serde::de::SeqAccess::next_element::<
                        Option<NodeIndex<Ix>>,
                    >(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                3usize,
                                &"struct Statechart with 4 elements",
                            ));
                        }
                    };
                    _serde::export::Ok(Statechart {
                        id: __field0,
                        initial: __field1,
                        graph: __field2,
                        active_state: __field3,
                    })
                }
                #[inline]
                fn visit_map<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: _serde::export::Option<String> = _serde::export::None;
                    let mut __field1: _serde::export::Option<Initial<Ix>> = _serde::export::None;
                    let mut __field2: _serde::export::Option<
                        StableDiGraph<State<Ix>, Transition<E>, Ix>,
                    > = _serde::export::None;
                    let mut __field3: _serde::export::Option<Option<NodeIndex<Ix>>> =
                        _serde::export::None;
                    while let _serde::export::Some(__key) =
                        match _serde::de::MapAccess::next_key::<__Field>(&mut __map) {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        }
                    {
                        match __key {
                            __Field::__field0 => {
                                if _serde::export::Option::is_some(&__field0) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("id"),
                                    );
                                }
                                __field0 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<String>(&mut __map) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field1 => {
                                if _serde::export::Option::is_some(&__field1) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "initial",
                                        ),
                                    );
                                }
                                __field1 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<Initial<Ix>>(
                                        &mut __map,
                                    ) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field2 => {
                                if _serde::export::Option::is_some(&__field2) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("graph"),
                                    );
                                }
                                __field2 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<
                                        StableDiGraph<State<Ix>, Transition<E>, Ix>,
                                    >(&mut __map)
                                    {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field3 => {
                                if _serde::export::Option::is_some(&__field3) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "active_state",
                                        ),
                                    );
                                }
                                __field3 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<Option<NodeIndex<Ix>>>(
                                        &mut __map,
                                    ) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            _ => {
                                let _ = match _serde::de::MapAccess::next_value::<
                                    _serde::de::IgnoredAny,
                                >(&mut __map)
                                {
                                    _serde::export::Ok(__val) => __val,
                                    _serde::export::Err(__err) => {
                                        return _serde::export::Err(__err);
                                    }
                                };
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        _serde::export::Some(__field0) => __field0,
                        _serde::export::None => match _serde::private::de::missing_field("id") {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field1 = match __field1 {
                        _serde::export::Some(__field1) => __field1,
                        _serde::export::None => match _serde::private::de::missing_field("initial")
                        {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field2 = match __field2 {
                        _serde::export::Some(__field2) => __field2,
                        _serde::export::None => match _serde::private::de::missing_field("graph") {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field3 = match __field3 {
                        _serde::export::Some(__field3) => __field3,
                        _serde::export::None => {
                            match _serde::private::de::missing_field("active_state") {
                                _serde::export::Ok(__val) => __val,
                                _serde::export::Err(__err) => {
                                    return _serde::export::Err(__err);
                                }
                            }
                        }
                    };
                    _serde::export::Ok(Statechart {
                        id: __field0,
                        initial: __field1,
                        graph: __field2,
                        active_state: __field3,
                    })
                }
            }
            const FIELDS: &'static [&'static str] = &["id", "initial", "graph", "active_state"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "Statechart",
                FIELDS,
                __Visitor {
                    marker: _serde::export::PhantomData::<Statechart<E, Ix>>,
                    lifetime: _serde::export::PhantomData,
                },
            )
        }
    }
};
pub enum Initial<Ix: IndexType> {
    None,
    Initial(NodeIndex<Ix>),
    History(NodeIndex<Ix>),
}
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_Initial: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<Ix: IndexType> _serde::Serialize for Initial<Ix>
    where
        Ix: _serde::Serialize,
    {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            match *self {
                Initial::None => _serde::Serializer::serialize_unit_variant(
                    __serializer,
                    "Initial",
                    0u32,
                    "None",
                ),
                Initial::Initial(ref __field0) => _serde::Serializer::serialize_newtype_variant(
                    __serializer,
                    "Initial",
                    1u32,
                    "Initial",
                    __field0,
                ),
                Initial::History(ref __field0) => _serde::Serializer::serialize_newtype_variant(
                    __serializer,
                    "Initial",
                    2u32,
                    "History",
                    __field0,
                ),
            }
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_Initial: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de, Ix: IndexType> _serde::Deserialize<'de> for Initial<Ix>
    where
        Ix: _serde::Deserialize<'de>,
    {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
            }
            struct __FieldVisitor;
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "variant identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => _serde::export::Ok(__Field::__field0),
                        1u64 => _serde::export::Ok(__Field::__field1),
                        2u64 => _serde::export::Ok(__Field::__field2),
                        _ => _serde::export::Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"variant index 0 <= i < 3",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "None" => _serde::export::Ok(__Field::__field0),
                        "Initial" => _serde::export::Ok(__Field::__field1),
                        "History" => _serde::export::Ok(__Field::__field2),
                        _ => _serde::export::Err(_serde::de::Error::unknown_variant(
                            __value, VARIANTS,
                        )),
                    }
                }
                fn visit_bytes<__E>(
                    self,
                    __value: &[u8],
                ) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"None" => _serde::export::Ok(__Field::__field0),
                        b"Initial" => _serde::export::Ok(__Field::__field1),
                        b"History" => _serde::export::Ok(__Field::__field2),
                        _ => {
                            let __value = &_serde::export::from_utf8_lossy(__value);
                            _serde::export::Err(_serde::de::Error::unknown_variant(
                                __value, VARIANTS,
                            ))
                        }
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de, Ix: IndexType>
            where
                Ix: _serde::Deserialize<'de>,
            {
                marker: _serde::export::PhantomData<Initial<Ix>>,
                lifetime: _serde::export::PhantomData<&'de ()>,
            }
            impl<'de, Ix: IndexType> _serde::de::Visitor<'de> for __Visitor<'de, Ix>
            where
                Ix: _serde::Deserialize<'de>,
            {
                type Value = Initial<Ix>;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "enum Initial")
                }
                fn visit_enum<__A>(
                    self,
                    __data: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::EnumAccess<'de>,
                {
                    match match _serde::de::EnumAccess::variant(__data) {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        (__Field::__field0, __variant) => {
                            match _serde::de::VariantAccess::unit_variant(__variant) {
                                _serde::export::Ok(__val) => __val,
                                _serde::export::Err(__err) => {
                                    return _serde::export::Err(__err);
                                }
                            };
                            _serde::export::Ok(Initial::None)
                        }
                        (__Field::__field1, __variant) => _serde::export::Result::map(
                            _serde::de::VariantAccess::newtype_variant::<NodeIndex<Ix>>(__variant),
                            Initial::Initial,
                        ),
                        (__Field::__field2, __variant) => _serde::export::Result::map(
                            _serde::de::VariantAccess::newtype_variant::<NodeIndex<Ix>>(__variant),
                            Initial::History,
                        ),
                    }
                }
            }
            const VARIANTS: &'static [&'static str] = &["None", "Initial", "History"];
            _serde::Deserializer::deserialize_enum(
                __deserializer,
                "Initial",
                VARIANTS,
                __Visitor {
                    marker: _serde::export::PhantomData::<Initial<Ix>>,
                    lifetime: _serde::export::PhantomData,
                },
            )
        }
    }
};
pub struct State<Ix: IndexType> {
    pub id: String,
    pub initial: Initial<Ix>,
    pub context: Box<dyn StateContext>,
    pub parent: Option<NodeIndex<Ix>>,
}
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_State: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<Ix: IndexType> _serde::Serialize for State<Ix>
    where
        Ix: _serde::Serialize,
    {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = match _serde::Serializer::serialize_struct(
                __serializer,
                "State",
                false as usize + 1 + 1 + 1 + 1,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "id", &self.id)
            {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "initial",
                &self.initial,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "context",
                &self.context,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "parent",
                &self.parent,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_State: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de, Ix: IndexType> _serde::Deserialize<'de> for State<Ix>
    where
        Ix: _serde::Deserialize<'de>,
    {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __field3,
                __ignore,
            }
            struct __FieldVisitor;
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => _serde::export::Ok(__Field::__field0),
                        1u64 => _serde::export::Ok(__Field::__field1),
                        2u64 => _serde::export::Ok(__Field::__field2),
                        3u64 => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 4",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "id" => _serde::export::Ok(__Field::__field0),
                        "initial" => _serde::export::Ok(__Field::__field1),
                        "context" => _serde::export::Ok(__Field::__field2),
                        "parent" => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(
                    self,
                    __value: &[u8],
                ) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"id" => _serde::export::Ok(__Field::__field0),
                        b"initial" => _serde::export::Ok(__Field::__field1),
                        b"context" => _serde::export::Ok(__Field::__field2),
                        b"parent" => _serde::export::Ok(__Field::__field3),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de, Ix: IndexType>
            where
                Ix: _serde::Deserialize<'de>,
            {
                marker: _serde::export::PhantomData<State<Ix>>,
                lifetime: _serde::export::PhantomData<&'de ()>,
            }
            impl<'de, Ix: IndexType> _serde::de::Visitor<'de> for __Visitor<'de, Ix>
            where
                Ix: _serde::Deserialize<'de>,
            {
                type Value = State<Ix>;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "struct State")
                }
                #[inline]
                fn visit_seq<__A>(
                    self,
                    mut __seq: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match match _serde::de::SeqAccess::next_element::<String>(&mut __seq) {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        } {
                            _serde::export::Some(__value) => __value,
                            _serde::export::None => {
                                return _serde::export::Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct State with 4 elements",
                                ));
                            }
                        };
                    let __field1 = match match _serde::de::SeqAccess::next_element::<Initial<Ix>>(
                        &mut __seq,
                    ) {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct State with 4 elements",
                            ));
                        }
                    };
                    let __field2 = match match _serde::de::SeqAccess::next_element::<
                        Box<dyn StateContext>,
                    >(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct State with 4 elements",
                            ));
                        }
                    };
                    let __field3 = match match _serde::de::SeqAccess::next_element::<
                        Option<NodeIndex<Ix>>,
                    >(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                3usize,
                                &"struct State with 4 elements",
                            ));
                        }
                    };
                    _serde::export::Ok(State {
                        id: __field0,
                        initial: __field1,
                        context: __field2,
                        parent: __field3,
                    })
                }
                #[inline]
                fn visit_map<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: _serde::export::Option<String> = _serde::export::None;
                    let mut __field1: _serde::export::Option<Initial<Ix>> = _serde::export::None;
                    let mut __field2: _serde::export::Option<Box<dyn StateContext>> =
                        _serde::export::None;
                    let mut __field3: _serde::export::Option<Option<NodeIndex<Ix>>> =
                        _serde::export::None;
                    while let _serde::export::Some(__key) =
                        match _serde::de::MapAccess::next_key::<__Field>(&mut __map) {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        }
                    {
                        match __key {
                            __Field::__field0 => {
                                if _serde::export::Option::is_some(&__field0) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("id"),
                                    );
                                }
                                __field0 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<String>(&mut __map) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field1 => {
                                if _serde::export::Option::is_some(&__field1) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "initial",
                                        ),
                                    );
                                }
                                __field1 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<Initial<Ix>>(
                                        &mut __map,
                                    ) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field2 => {
                                if _serde::export::Option::is_some(&__field2) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "context",
                                        ),
                                    );
                                }
                                __field2 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<Box<dyn StateContext>>(
                                        &mut __map,
                                    ) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field3 => {
                                if _serde::export::Option::is_some(&__field3) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "parent",
                                        ),
                                    );
                                }
                                __field3 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<Option<NodeIndex<Ix>>>(
                                        &mut __map,
                                    ) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            _ => {
                                let _ = match _serde::de::MapAccess::next_value::<
                                    _serde::de::IgnoredAny,
                                >(&mut __map)
                                {
                                    _serde::export::Ok(__val) => __val,
                                    _serde::export::Err(__err) => {
                                        return _serde::export::Err(__err);
                                    }
                                };
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        _serde::export::Some(__field0) => __field0,
                        _serde::export::None => match _serde::private::de::missing_field("id") {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field1 = match __field1 {
                        _serde::export::Some(__field1) => __field1,
                        _serde::export::None => match _serde::private::de::missing_field("initial")
                        {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field2 = match __field2 {
                        _serde::export::Some(__field2) => __field2,
                        _serde::export::None => match _serde::private::de::missing_field("context")
                        {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field3 = match __field3 {
                        _serde::export::Some(__field3) => __field3,
                        _serde::export::None => {
                            match _serde::private::de::missing_field("parent") {
                                _serde::export::Ok(__val) => __val,
                                _serde::export::Err(__err) => {
                                    return _serde::export::Err(__err);
                                }
                            }
                        }
                    };
                    _serde::export::Ok(State {
                        id: __field0,
                        initial: __field1,
                        context: __field2,
                        parent: __field3,
                    })
                }
            }
            const FIELDS: &'static [&'static str] = &["id", "initial", "context", "parent"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "State",
                FIELDS,
                __Visitor {
                    marker: _serde::export::PhantomData::<State<Ix>>,
                    lifetime: _serde::export::PhantomData,
                },
            )
        }
    }
};
pub trait StateContext: typetag::Serialize + typetag::Deserialize {
    fn entry(&mut self) -> Result<()> {
        Ok(())
    }
    fn exit(&mut self) -> Result<()> {
        Ok(())
    }
    #[doc(hidden)]
    fn typetag_name(&self) -> &'static str;
    #[doc(hidden)]
    fn typetag_deserialize(&self);
}
#[allow(non_upper_case_globals)]
const _StateContext_registry: () = {
    impl<'typetag> typetag::serde::Serialize for dyn StateContext + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            let name = Self::typetag_name(self);
            typetag::internally::serialize(serializer, "type", name, self)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn StateContext + Send + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn StateContext, serializer)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn StateContext + Sync + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn StateContext, serializer)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn StateContext + Send + Sync + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn StateContext, serializer)
        }
    }
    type TypetagStrictest = <dyn StateContext as typetag::Strictest>::Object;
    type TypetagFn = typetag::DeserializeFn<TypetagStrictest>;
    pub struct TypetagRegistration {
        name: &'static str,
        deserializer: TypetagFn,
    }
    impl ::inventory::Collect for TypetagRegistration {
        #[inline]
        fn registry() -> &'static ::inventory::Registry<Self> {
            static REGISTRY: ::inventory::Registry<TypetagRegistration> =
                ::inventory::Registry::new();
            &REGISTRY
        }
    }
    impl dyn StateContext {
        #[doc(hidden)]
        pub fn typetag_register(
            name: &'static str,
            deserializer: TypetagFn,
        ) -> TypetagRegistration {
            TypetagRegistration { name, deserializer }
        }
    }
    #[allow(missing_copy_implementations)]
    #[allow(non_camel_case_types)]
    #[allow(dead_code)]
    struct TYPETAG {
        __private_field: (),
    }
    #[doc(hidden)]
    static TYPETAG: TYPETAG = TYPETAG {
        __private_field: (),
    };
    impl ::lazy_static::__Deref for TYPETAG {
        type Target = typetag::Registry<TypetagStrictest>;
        fn deref(&self) -> &typetag::Registry<TypetagStrictest> {
            #[inline(always)]
            fn __static_ref_initialize() -> typetag::Registry<TypetagStrictest> {
                {
                    let mut map = std::collections::BTreeMap::new();
                    let mut names = std::vec::Vec::new();
                    for registered in typetag::inventory::iter::<TypetagRegistration> {
                        match map.entry(registered.name) {
                            std::collections::btree_map::Entry::Vacant(entry) => {
                                entry.insert(std::option::Option::Some(registered.deserializer));
                            }
                            std::collections::btree_map::Entry::Occupied(mut entry) => {
                                entry.insert(std::option::Option::None);
                            }
                        }
                        names.push(registered.name);
                    }
                    names.sort_unstable();
                    typetag::Registry { map, names }
                }
            }
            #[inline(always)]
            fn __stability() -> &'static typetag::Registry<TypetagStrictest> {
                static LAZY: ::lazy_static::lazy::Lazy<typetag::Registry<TypetagStrictest>> =
                    ::lazy_static::lazy::Lazy::INIT;
                LAZY.get(__static_ref_initialize)
            }
            __stability()
        }
    }
    impl ::lazy_static::LazyStatic for TYPETAG {
        fn initialize(lazy: &Self) {
            let _ = &**lazy;
        }
    }
    impl typetag::Strictest for dyn StateContext {
        type Object = dyn StateContext;
    }
    impl<'de> typetag::serde::Deserialize<'de> for std::boxed::Box<dyn StateContext> {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: typetag::serde::Deserializer<'de>,
        {
            typetag::internally::deserialize(deserializer, "StateContext", "type", &TYPETAG)
        }
    }
};
pub struct DefaultStateContext;
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_DefaultStateContext: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl _serde::Serialize for DefaultStateContext {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            _serde::Serializer::serialize_unit_struct(__serializer, "DefaultStateContext")
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_DefaultStateContext: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for DefaultStateContext {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            struct __Visitor;
            impl<'de> _serde::de::Visitor<'de> for __Visitor {
                type Value = DefaultStateContext;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(
                        __formatter,
                        "unit struct DefaultStateContext",
                    )
                }
                #[inline]
                fn visit_unit<__E>(self) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    _serde::export::Ok(DefaultStateContext)
                }
            }
            _serde::Deserializer::deserialize_unit_struct(
                __deserializer,
                "DefaultStateContext",
                __Visitor,
            )
        }
    }
};
impl StateContext for DefaultStateContext {
    #[doc(hidden)]
    fn typetag_name(&self) -> &'static str {
        "DefaultStateContext"
    }
    #[doc(hidden)]
    fn typetag_deserialize(&self) {}
}
#[used]
#[allow(non_upper_case_globals)]
#[link_section = ".init_array"]
#[allow(non_upper_case_globals)]
static __init11451125888343693689: extern "C" fn() = {
    #[link_section = ".text.startup"]
    extern "C" fn __init11451125888343693689() {
        typetag::inventory::submit({
            <dyn StateContext>::typetag_register("DefaultStateContext", |deserializer| {
                std::result::Result::Ok(std::boxed::Box::new(typetag::erased_serde::deserialize::<
                    DefaultStateContext,
                >(deserializer)?))
            })
        });
    }
    __init11451125888343693689
};
pub trait TransitionContext: typetag::Serialize + typetag::Deserialize {
    fn action(&mut self) -> Result<()> {
        Ok(())
    }
    fn guard(&self) -> bool {
        true
    }
    #[doc(hidden)]
    fn typetag_name(&self) -> &'static str;
    #[doc(hidden)]
    fn typetag_deserialize(&self);
}
#[allow(non_upper_case_globals)]
const _TransitionContext_registry: () = {
    impl<'typetag> typetag::serde::Serialize for dyn TransitionContext + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            let name = Self::typetag_name(self);
            typetag::internally::serialize(serializer, "type", name, self)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn TransitionContext + Send + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn TransitionContext, serializer)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn TransitionContext + Sync + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn TransitionContext, serializer)
        }
    }
    impl<'typetag> typetag::serde::Serialize for dyn TransitionContext + Send + Sync + 'typetag {
        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: typetag::serde::Serializer,
        {
            typetag::serde::Serialize::serialize(self as &dyn TransitionContext, serializer)
        }
    }
    type TypetagStrictest = <dyn TransitionContext as typetag::Strictest>::Object;
    type TypetagFn = typetag::DeserializeFn<TypetagStrictest>;
    pub struct TypetagRegistration {
        name: &'static str,
        deserializer: TypetagFn,
    }
    impl ::inventory::Collect for TypetagRegistration {
        #[inline]
        fn registry() -> &'static ::inventory::Registry<Self> {
            static REGISTRY: ::inventory::Registry<TypetagRegistration> =
                ::inventory::Registry::new();
            &REGISTRY
        }
    }
    impl dyn TransitionContext {
        #[doc(hidden)]
        pub fn typetag_register(
            name: &'static str,
            deserializer: TypetagFn,
        ) -> TypetagRegistration {
            TypetagRegistration { name, deserializer }
        }
    }
    #[allow(missing_copy_implementations)]
    #[allow(non_camel_case_types)]
    #[allow(dead_code)]
    struct TYPETAG {
        __private_field: (),
    }
    #[doc(hidden)]
    static TYPETAG: TYPETAG = TYPETAG {
        __private_field: (),
    };
    impl ::lazy_static::__Deref for TYPETAG {
        type Target = typetag::Registry<TypetagStrictest>;
        fn deref(&self) -> &typetag::Registry<TypetagStrictest> {
            #[inline(always)]
            fn __static_ref_initialize() -> typetag::Registry<TypetagStrictest> {
                {
                    let mut map = std::collections::BTreeMap::new();
                    let mut names = std::vec::Vec::new();
                    for registered in typetag::inventory::iter::<TypetagRegistration> {
                        match map.entry(registered.name) {
                            std::collections::btree_map::Entry::Vacant(entry) => {
                                entry.insert(std::option::Option::Some(registered.deserializer));
                            }
                            std::collections::btree_map::Entry::Occupied(mut entry) => {
                                entry.insert(std::option::Option::None);
                            }
                        }
                        names.push(registered.name);
                    }
                    names.sort_unstable();
                    typetag::Registry { map, names }
                }
            }
            #[inline(always)]
            fn __stability() -> &'static typetag::Registry<TypetagStrictest> {
                static LAZY: ::lazy_static::lazy::Lazy<typetag::Registry<TypetagStrictest>> =
                    ::lazy_static::lazy::Lazy::INIT;
                LAZY.get(__static_ref_initialize)
            }
            __stability()
        }
    }
    impl ::lazy_static::LazyStatic for TYPETAG {
        fn initialize(lazy: &Self) {
            let _ = &**lazy;
        }
    }
    impl typetag::Strictest for dyn TransitionContext {
        type Object = dyn TransitionContext;
    }
    impl<'de> typetag::serde::Deserialize<'de> for std::boxed::Box<dyn TransitionContext> {
        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
        where
            D: typetag::serde::Deserializer<'de>,
        {
            typetag::internally::deserialize(deserializer, "TransitionContext", "type", &TYPETAG)
        }
    }
};
pub struct DefaultTransitionContext;
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_DefaultTransitionContext: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl _serde::Serialize for DefaultTransitionContext {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            _serde::Serializer::serialize_unit_struct(__serializer, "DefaultTransitionContext")
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_DefaultTransitionContext: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de> _serde::Deserialize<'de> for DefaultTransitionContext {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            struct __Visitor;
            impl<'de> _serde::de::Visitor<'de> for __Visitor {
                type Value = DefaultTransitionContext;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(
                        __formatter,
                        "unit struct DefaultTransitionContext",
                    )
                }
                #[inline]
                fn visit_unit<__E>(self) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    _serde::export::Ok(DefaultTransitionContext)
                }
            }
            _serde::Deserializer::deserialize_unit_struct(
                __deserializer,
                "DefaultTransitionContext",
                __Visitor,
            )
        }
    }
};
impl TransitionContext for DefaultTransitionContext {
    #[doc(hidden)]
    fn typetag_name(&self) -> &'static str {
        "DefaultTransitionContext"
    }
    #[doc(hidden)]
    fn typetag_deserialize(&self) {}
}
#[used]
#[allow(non_upper_case_globals)]
#[link_section = ".init_array"]
#[allow(non_upper_case_globals)]
static __init17230200109763255570: extern "C" fn() = {
    #[link_section = ".text.startup"]
    extern "C" fn __init17230200109763255570() {
        typetag::inventory::submit({
            <dyn TransitionContext>::typetag_register("DefaultTransitionContext", |deserializer| {
                std::result::Result::Ok(std::boxed::Box::new(typetag::erased_serde::deserialize::<
                    DefaultTransitionContext,
                >(deserializer)?))
            })
        });
    }
    __init17230200109763255570
};
pub struct Transition<E> {
    pub event: E,
    pub context: Box<dyn TransitionContext>,
}
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_SERIALIZE_FOR_Transition: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<E> _serde::Serialize for Transition<E>
    where
        E: _serde::Serialize,
    {
        fn serialize<__S>(&self, __serializer: __S) -> _serde::export::Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = match _serde::Serializer::serialize_struct(
                __serializer,
                "Transition",
                false as usize + 1 + 1,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "event",
                &self.event,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            match _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "context",
                &self.context,
            ) {
                _serde::export::Ok(__val) => __val,
                _serde::export::Err(__err) => {
                    return _serde::export::Err(__err);
                }
            };
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
};
#[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
const _IMPL_DESERIALIZE_FOR_Transition: () = {
    #[allow(unknown_lints)]
    #[allow(rust_2018_idioms)]
    extern crate serde as _serde;
    #[automatically_derived]
    impl<'de, E> _serde::Deserialize<'de> for Transition<E>
    where
        E: _serde::Deserialize<'de>,
    {
        fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __ignore,
            }
            struct __FieldVisitor;
            impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                type Value = __Field;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => _serde::export::Ok(__Field::__field0),
                        1u64 => _serde::export::Ok(__Field::__field1),
                        _ => _serde::export::Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 2",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "event" => _serde::export::Ok(__Field::__field0),
                        "context" => _serde::export::Ok(__Field::__field1),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(
                    self,
                    __value: &[u8],
                ) -> _serde::export::Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"event" => _serde::export::Ok(__Field::__field0),
                        b"context" => _serde::export::Ok(__Field::__field1),
                        _ => _serde::export::Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> _serde::export::Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de, E>
            where
                E: _serde::Deserialize<'de>,
            {
                marker: _serde::export::PhantomData<Transition<E>>,
                lifetime: _serde::export::PhantomData<&'de ()>,
            }
            impl<'de, E> _serde::de::Visitor<'de> for __Visitor<'de, E>
            where
                E: _serde::Deserialize<'de>,
            {
                type Value = Transition<E>;
                fn expecting(
                    &self,
                    __formatter: &mut _serde::export::Formatter,
                ) -> _serde::export::fmt::Result {
                    _serde::export::Formatter::write_str(__formatter, "struct Transition")
                }
                #[inline]
                fn visit_seq<__A>(
                    self,
                    mut __seq: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 = match match _serde::de::SeqAccess::next_element::<E>(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                0usize,
                                &"struct Transition with 2 elements",
                            ));
                        }
                    };
                    let __field1 = match match _serde::de::SeqAccess::next_element::<
                        Box<dyn TransitionContext>,
                    >(&mut __seq)
                    {
                        _serde::export::Ok(__val) => __val,
                        _serde::export::Err(__err) => {
                            return _serde::export::Err(__err);
                        }
                    } {
                        _serde::export::Some(__value) => __value,
                        _serde::export::None => {
                            return _serde::export::Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct Transition with 2 elements",
                            ));
                        }
                    };
                    _serde::export::Ok(Transition {
                        event: __field0,
                        context: __field1,
                    })
                }
                #[inline]
                fn visit_map<__A>(
                    self,
                    mut __map: __A,
                ) -> _serde::export::Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: _serde::export::Option<E> = _serde::export::None;
                    let mut __field1: _serde::export::Option<Box<dyn TransitionContext>> =
                        _serde::export::None;
                    while let _serde::export::Some(__key) =
                        match _serde::de::MapAccess::next_key::<__Field>(&mut __map) {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        }
                    {
                        match __key {
                            __Field::__field0 => {
                                if _serde::export::Option::is_some(&__field0) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("event"),
                                    );
                                }
                                __field0 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<E>(&mut __map) {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            __Field::__field1 => {
                                if _serde::export::Option::is_some(&__field1) {
                                    return _serde::export::Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "context",
                                        ),
                                    );
                                }
                                __field1 = _serde::export::Some(
                                    match _serde::de::MapAccess::next_value::<
                                        Box<dyn TransitionContext>,
                                    >(&mut __map)
                                    {
                                        _serde::export::Ok(__val) => __val,
                                        _serde::export::Err(__err) => {
                                            return _serde::export::Err(__err);
                                        }
                                    },
                                );
                            }
                            _ => {
                                let _ = match _serde::de::MapAccess::next_value::<
                                    _serde::de::IgnoredAny,
                                >(&mut __map)
                                {
                                    _serde::export::Ok(__val) => __val,
                                    _serde::export::Err(__err) => {
                                        return _serde::export::Err(__err);
                                    }
                                };
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        _serde::export::Some(__field0) => __field0,
                        _serde::export::None => match _serde::private::de::missing_field("event") {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    let __field1 = match __field1 {
                        _serde::export::Some(__field1) => __field1,
                        _serde::export::None => match _serde::private::de::missing_field("context")
                        {
                            _serde::export::Ok(__val) => __val,
                            _serde::export::Err(__err) => {
                                return _serde::export::Err(__err);
                            }
                        },
                    };
                    _serde::export::Ok(Transition {
                        event: __field0,
                        context: __field1,
                    })
                }
            }
            const FIELDS: &'static [&'static str] = &["event", "context"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "Transition",
                FIELDS,
                __Visitor {
                    marker: _serde::export::PhantomData::<Transition<E>>,
                    lifetime: _serde::export::PhantomData,
                },
            )
        }
    }
};
impl<E, Ix: IndexType> Statechart<E, Ix> {
    pub fn new(id: &str) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            graph: StableDiGraph::default(),
            active_state: None,
        }
    }
    pub fn export(&self) -> Result<String> {
        Ok(to_string_pretty(&self, PrettyConfig::default())?)
    }
    pub fn get(&self, i: NodeIndex<Ix>) -> &State<Ix> {
        &self.graph[i]
    }
    pub fn get_mut(&mut self, i: NodeIndex<Ix>) -> &mut State<Ix> {
        &mut self.graph[i]
    }
    pub fn add_state(&mut self, s: State<Ix>) -> Result<NodeIndex<Ix>> {
        Ok(self.graph.add_node(s))
    }
    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }
    pub fn add_transition<C: 'static + TransitionContext>(
        &mut self,
        a: NodeIndex<Ix>,
        b: NodeIndex<Ix>,
        event: E,
        ctx: C,
    ) -> Result<()> {
        self.graph
            .add_edge(a, b, Transition::new(event, Box::new(ctx)));
        Ok(())
    }
    pub fn transition(&mut self, _event: E) -> Result<()> {
        Ok(())
    }
}
impl<Ix: IndexType> State<Ix> {
    pub fn new<C: 'static + StateContext>(id: &str, ctx: C, parent: Option<NodeIndex<Ix>>) -> Self {
        Self {
            id: id.to_string(),
            initial: Initial::None,
            context: Box::new(ctx),
            parent: parent,
        }
    }
    pub fn set_initial(&mut self, initial: Initial<Ix>) -> Result<()> {
        self.initial = initial;
        Ok(())
    }
}
impl<E> Transition<E> {
    pub fn new(event: E, context: Box<dyn TransitionContext>) -> Self {
        Self { event, context }
    }
}
