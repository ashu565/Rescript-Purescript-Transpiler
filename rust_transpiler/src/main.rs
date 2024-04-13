mod parsetree;
use parsetree::purescript::purescript_function;
use parsetree::rescript::rescript_function;
use parsetree::rescript::types::Structure;
use parsetree::purescript::types::Module;
use std::fs::File;
use std::io::BufReader;
use serde_json;
use serde::{Serialize, Deserialize};



#[derive(Debug, PartialEq,  PartialOrd)]
#[derive(Serialize, Deserialize)]
#[serde(tag = "tag")]
enum Test1 {
    Cons1(i32),
    Cons2(i64)
}

fn main() {
    // purescript_cst() // uncomment to use
    rescript_cst() // uncomment to use
}

fn purescript_cst() {
    println!("Hello, world!");
    let mut s= String::from("Hello");

    s.push_str(", Again");
    test(&mut s);
    println!("{s}");

    println!("Testing Modules");

    let file = File::open("./purescript_pt.json").expect("Failed to open file");

    let reader = BufReader::new(file);

    let data: Module<Vec<i64>> = serde_json::from_reader(reader).expect("Failed to deserialize JSON");

    // let data2: Vec<Test1> = vec![Test1::Cons1(32), Test1::Cons2(23)];
    // // Serialize the `data2` variable to a JSON string
    // let serialized_data = serde_json::to_string(&data2).expect("Failed to serialize data");

    // Print the serialized JSON string to see the contents
    // println!("Serialized data2: {}", serialized_data);

    purescript_function();
    rescript_function();
}

fn rescript_cst() {
    println!("Hello, world!");
    let mut s= String::from("Hello");

    s.push_str(", Again");
    test(&mut s);
    println!("{s}");

    println!("Testing Modules");

    let file = File::open("./sample_rescript.json").expect("Failed to open file");

    let reader = BufReader::new(file);

    let data: Structure = serde_json::from_reader(reader).expect("Failed to deserialize JSON");

    // let data2: Vec<Test1> = vec![Test1::Cons1(32), Test1::Cons2(23)];
    // // Serialize the `data2` variable to a JSON string
    // let serialized_data = serde_json::to_string(&data2).expect("Failed to serialize data");

    // Print the serialized JSON string to see the contents
    // println!("Serialized data2: {}", serialized_data);

    purescript_function();
    rescript_function();
}

fn test(some_string: &mut String) {
    some_string.push_str(" World");
    println!("{some_string}");
}
