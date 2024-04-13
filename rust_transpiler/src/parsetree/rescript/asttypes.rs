#![allow(non_snake_case)]

enum Constant {
    ConstInt(i32),
    ConstChar(char),
    ConstString(String, Option<String>),
    ConstFloat(String),
    ConstInt32(i32),
    ConstInt64(i64),
    ConstNativeInt(isize),
}

enum RecFlag {
    Nonrecursive,
    Recursive,
}

enum DirectionFlag {
    Upto,
    Downto,
}

enum PrivateFlag {
    Private,
    Public,
}

enum MutableFlag {
    Immutable,
    Mutable,
}

enum VirtualFlag {
    Virtual,
    Concrete,
}

enum OverrideFlag {
    Override,
    Fresh,
}

enum ClosedFlag {
    Closed,
    Open,
}

// In Rust, a type alias can be used for simple renaming.
type Label = String;

enum ArgLabel {
    Nolabel,
    Labelled(String), // label:T -> ...
    Optional(String), // ?label:T -> ...
}

struct Loc<T> {
    txt: T,
    // Rust does not have a direct equivalent to OCaml's Location.t type,
    // so you would need to define it or use a placeholder type.
    // Here, we'll assume a simple struct for demonstration.
    loc: Location,
}

struct Location {
    // Placeholder fields for the location. In practice, you'll define these according to your needs.
    file: String,
    line: i32,
    column: i32,
}

enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}
