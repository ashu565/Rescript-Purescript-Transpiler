#![allow(dead_code)]
#![allow(non_snake_case)]
use serde::{Serialize, Serializer, Deserializer, Deserialize};
use core::str;
use std::{fmt::Debug, marker::PhantomData};
use serde::de::{self, Visitor, SeqAccess};
use std::fmt;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
pub struct ProperNameData {
    pub runProperName: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProperName<A> {
    ProperName(ProperNameData, PhantomData<A>),
}

impl<T> Serialize for ProperName<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            ProperName::ProperName(ref data, _) => serializer.serialize_str(&data.runProperName),
        }
    }
}

impl<'de, T> Deserialize<'de> for ProperName<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StringVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for StringVisitor<T> {
            type Value = ProperName<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<ProperName<T>, E>
            where
                E: de::Error,
            {
                Ok(ProperName::ProperName(ProperNameData { runProperName: v.to_owned() }, PhantomData))
            }
        }

        deserializer.deserialize_str(StringVisitor(PhantomData))
    }
}




#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum ProperNameType {
    TypeName,
    ConstructorName,
    ClassName,
    Namespace,
}

// OpName

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
pub struct OpNameData {
    pub runOpName: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpName<A> {
    OpName(OpNameData, PhantomData<A>),
}
impl<T> Serialize for OpName<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            OpName::OpName(ref data, _) => serializer.serialize_str(&data.runOpName),
        }
    }
}

impl<'de, T> Deserialize<'de> for OpName<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StringVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for StringVisitor<T> {
            type Value = OpName<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<OpName<T>, E>
            where
                E: de::Error,
            {
                Ok(OpName::OpName(OpNameData { runOpName: v.to_owned() }, PhantomData))
            }
        }

        deserializer.deserialize_str(StringVisitor(PhantomData))
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag", content = "contents")]
pub enum OpNameType {
    ValueOpName,
    TypeOpName,
    AnyOpName,
}

// ModuleName

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModuleName {
    ModuleName(String),
}

impl Serialize for ModuleName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            ModuleName::ModuleName(name) => {
                // Split the name on "." and serialize as a sequence
                let parts: Vec<&str> = name.split('.').collect();
                // Directly serialize the vector of string slices
                serializer.collect_seq(parts)
            },
        }
    }
}


impl<'de> Deserialize<'de> for ModuleName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ModuleNameVisitor;

        impl<'de> Visitor<'de> for ModuleNameVisitor {
            type Value = ModuleName;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a sequence of strings representing module name parts")
            }

            // Handle a sequence of strings
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut parts: Vec<String> = Vec::new();

                // Manually iterate over the sequence
                while let Some(part) = seq.next_element()? {
                    parts.push(part);
                }

                // Join the parts with "."
                let joined_parts = parts.join(".");
                Ok(ModuleName::ModuleName(joined_parts))
            }
        }

        deserializer.deserialize_seq(ModuleNameVisitor)
    }
}

// PSString

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
pub struct PSStringData {
    pub toUTF16CodeUnits: Vec<u16>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)] // TODO - Have to handle custom serialization when needed
pub enum PSString {
    PSString(PSStringData),
}


impl<'de> Deserialize<'de> for PSString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct PSStringVisitor;

        impl<'de> Visitor<'de> for PSStringVisitor {
            type Value = PSString;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string or an array of UTF-16 code units")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(PSString::PSString(PSStringData { toUTF16CodeUnits: value.encode_utf16().collect() }))
            }

            // Handle an array of UTF-16 code units
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut code_units = Vec::new();

                // Manually iterate over the sequence
                while let Some(code_unit) = seq.next_element()? {
                    code_units.push(code_unit);
                }

                Ok(PSString::PSString(PSStringData { toUTF16CodeUnits: code_units }))
            }
        }

        deserializer.deserialize_any(PSStringVisitor)
    }
}
