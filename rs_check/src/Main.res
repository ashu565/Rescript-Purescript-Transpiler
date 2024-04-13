@@uncurried
@@warning("-27-32-33-44")
open RescriptCore
open Ts2ocaml
/* import * as Data_Unit from "../Data.Unit/index.js"; */
module Data_Unit = Index.Export
module X = {
  type t = string
}
module AnonymousInterface1 = {
  type t
  @get external get_aa: (t) => string = "aa"
  @set external set_aa: (t, string) => unit = "aa"
  @get external get_bb: (t) => float = "bb"
  @set external set_bb: (t, float) => unit = "bb"
  @obj external make: (~aa:string, ~bb:float) => t = ""
}
module Name = {
  module AnonymousInterface1 = {
    type t
    @get external get_aa: (t) => string = "aa"
    @set external set_aa: (t, string) => unit = "aa"
    @get external get_bb: (t) => float = "bb"
    @set external set_bb: (t, float) => unit = "bb"
    @obj external make: (~aa:string, ~bb:float) => t = ""
  }
  type t = AnonymousInterface1.t
}
@val external name: (AnonymousInterface1.t) => Name.t = "Name"
@val external main: () => Data_Unit.Unit.t = "main"
@val external sum: (float) => (float) => float = "sum"
@val external headd: (array<float>) => float = "headd"
module Export = {
  module Name = Name
  module X = X
}