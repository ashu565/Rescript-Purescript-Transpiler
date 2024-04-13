type nestedName = {
  firstName : string;
  lastName : string;
}

let nestedName_to_yojson (nestedName : nestedName) : Yojson.Safe.t =
  `Assoc [
    ("firstName", `String nestedName.firstName);
    ("lastName", `String nestedName.lastName);
  ]

type person = {
  name : nestedName;
  age : int;
  }

let person_to_yojson (person : person) : Yojson.Safe.t =
  `Assoc [
    ("name", nestedName_to_yojson person.name);
    ("age", `Int person.age);
  ]

type constant =
  Pconst_integer of string * string option
  | Pconst_char of int
  | Pconst_string of string * string option
  | Pconst_float of string * string option
