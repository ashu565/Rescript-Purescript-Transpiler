(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary AST types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of int
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
[@@deriving to_yojson]

type rec_flag = Nonrecursive | Recursive [@@deriving to_yojson]

type direction_flag = Upto | Downto [@@deriving to_yojson]

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public [@@deriving to_yojson]

type mutable_flag = Immutable | Mutable [@@deriving to_yojson]

type virtual_flag = Virtual | Concrete [@@deriving to_yojson]

type override_flag = Override | Fresh [@@deriving to_yojson]

type closed_flag = Closed | Open [@@deriving to_yojson]

type label = string [@@deriving to_yojson]

type arg_label =
    Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)
[@@deriving to_yojson]

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}
[@@deriving to_yojson]


type variance =
  | Covariant
  | Contravariant
  | Invariant
[@@deriving to_yojson]

let same_arg_label (x : arg_label) y = 
  match x with 
  | Nolabel -> y = Nolabel
  | Labelled s ->
    begin match y with 
    | Labelled s0 -> s = s0 
    | _ -> false 
    end 
  | Optional s ->
      begin match y with 
      | Optional s0 -> s = s0
      | _ -> false  
      end  
