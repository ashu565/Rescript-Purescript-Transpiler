@@uncurried
@@warning("-27-32-33-44")
open RescriptCore
open Ts2ocaml
module Unit : {
  module AnonymousInterface0 : {
    type t
    @get @return(nullable) external \"get_$$pursType": (t) => option<[#"Data.Unit.Unit"]> = "$$pursType"
    @set external \"set_$$pursType": (t, option<[#"Data.Unit.Unit"]>) => unit = "$$pursType"
    @obj external make: (~\"$$pursType":[#"Data.Unit.Unit"]=?) => t = ""
  }
  type t = AnonymousInterface0.t
}
@val external unit: Unit.t = "unit"
module Export : { module Unit = Unit }