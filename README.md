# Rescript-ParseTree-Analyzer
Analysis of Rescript ParseTree

`/haskell_rescript_types` - Contains Rescript Parsetree types representation, toJSON and fromJSON for the types.

`/cst_viewer` - Rescript Compiler parts for generating rescript parsetree, pretty print, toJSON and fromJSON instances.

`/rust_transpiler` - Contains Purescript Types and Rescript parse tree Types in Rust along with there toJSON, fromJSON instances. 

`/ps_lib and /rs_check` - Tried using the purescript (ps_lib) functions and types to use in rescript (rs_check) using `purescript-tsd-gen` and `ts2ocaml` packages.

## Development

#### **Entering nix development environment (nix shell)**
```nix develop```

#### **Running the Haskell Project**
```cd haskell_rescript_types```

```cabal build```

```cabal run``` (This will just output Hello Haskell)

**Testing the types in repl**

```cabal repl```

All type definitions are in `app/RescriptParsetree.hs`

```importParseTree``` -> Imports the dummy rescript parsetree (rescript_parsetree.json) and parses it into the type.

```
ghci> import RescriptParsetree
ghci> importParseTree
"Parsed Success"
[StructureItem]
```

#### **Running the OCaml Project**

Setting up opam for OCaml packages

```cd cst_viewer```

```opam init```

```
Do you want opam to modify ~/.bash_profile? [N/y/f]
(default is 'no', use 'f' to choose a different file) y

A hook can be added to opam's init scripts to ensure that the shell remains in sync with the opam
environment when they are loaded. Set that up? [y/N] y
```

```opam switch create .```

```eval $(opam env)```

Building and executing the project

```dune build```

```dune exec ./bin/main.exe```
