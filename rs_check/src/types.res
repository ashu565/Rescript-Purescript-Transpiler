@@uncurried
@@warning("-27-32-33-44")
open RescriptCore
open Ts2ocaml
module IUser = {
  type t = intf<[#IUser]>
  type tags = [#IUser]
  type this<'tags> = intf<'tags> constraint 'tags = [> #IUser ]
  @get external get_id: (this<'tags>) => string = "id"
  @set external set_id: (this<'tags>, string) => unit = "id"
  @get external get_name: (this<'tags>) => string = "name"
  @set external set_name: (this<'tags>, string) => unit = "name"
  @get external get_age: (this<'tags>) => float = "age"
  @set external set_age: (this<'tags>, float) => unit = "age"
  @obj external make: (~id:string, ~name:string, ~age:float) => t = ""
  external castFrom: (this<'tags>) => t = "%identity"
}
module User = {
  type t = intf<[#IUser | #User]>
  type tags = [#IUser | #User]
  type this<'tags> = intf<'tags> constraint 'tags = [> #User ]
  @new external make: (~id:string, ~name:string, ~age:float) => t = "User"
  @send external greet: (this<'tags>) => /* FIXME: unknown type */any = "greet"
  external castFrom: (this<'tags>) => t = "%identity"
  external asIUser: (t) => IUser.t = "%identity"
}
module IPurs = {
  module AnonymousInterface0 = {
    type t
    @get external get_name: (t) => string = "name"
    @set external set_name: (t, string) => unit = "name"
    @get external get_org: (t) => string = "org"
    @set external set_org: (t, string) => unit = "org"
    @get external get_rank: (t) => float = "rank"
    @set external set_rank: (t, float) => unit = "rank"
    @obj external make: (~name:string, ~org:string, ~rank:float) => t = ""
  }
  type t = AnonymousInterface0.t
}
module IProduct = {
  type t = intf<[#IProduct]>
  type tags = [#IProduct]
  type this<'tags> = intf<'tags> constraint 'tags = [> #IProduct ]
  @get external get_id: (this<'tags>) => string = "id"
  @set external set_id: (this<'tags>, string) => unit = "id"
  @get external get_name: (this<'tags>) => string = "name"
  @set external set_name: (this<'tags>, string) => unit = "name"
  @get external get_price: (this<'tags>) => float = "price"
  @set external set_price: (this<'tags>, float) => unit = "price"
  @get @return(nullable) external get_description: (this<'tags>) => option<string> = "description"
  @set external set_description: (this<'tags>, option<string>) => unit = "description"
  @send external applyDiscount: (this<'tags>, ~discountCode:string) => float = "applyDiscount"
  external castFrom: (this<'tags>) => t = "%identity"
}
module IAdminUser = {
  type t = intf<[#IAdminUser | #IUser]>
  type tags = [#IAdminUser | #IUser]
  type this<'tags> = intf<'tags> constraint 'tags = [> #IAdminUser ]
  @get external get_permissions: (this<'tags>) => array<string> = "permissions"
  @set external set_permissions: (this<'tags>, array<string>) => unit = "permissions"
  @get external get_id: (this<'tags>) => string = "id"
  @set external set_id: (this<'tags>, string) => unit = "id"
  @get external get_name: (this<'tags>) => string = "name"
  @set external set_name: (this<'tags>, string) => unit = "name"
  @get external get_age: (this<'tags>) => float = "age"
  @set external set_age: (this<'tags>, float) => unit = "age"
  @obj external make: (~permissions:array<string>, ~id:string, ~name:string, ~age:float) => t = ""
  external castFrom: (this<'tags>) => t = "%identity"
  external asIUser: (t) => IUser.t = "%identity"
}
@val external check: (string) => float = "check"