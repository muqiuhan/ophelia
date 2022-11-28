(*****************************************************)
(* Token Types                                       *)
(*                                                   *)
(* EOF (end-of-file) token is used to indicate that  *)
(* there is no more input left for lexcial analysis  *)
(*****************************************************)

open Core

module Type = struct
  type token_type =
    | Integer
    | Plus
    | EOF
    | None
  [@@deriving show]

  type t = token_type [@@deriving show]

  let to_string : token_type -> string = show
end

type token =
  { token_type : Type.t
  ; value : string
  }
[@@deriving show]

let none = { token_type = Type.None; value = "" }

type t = token [@@deriving show]

(***********************************************)
(* String representation of the token instance *)
(* Examples:                                   *)
(*   Token.{_type = Integer; value = "3"}      *)
(*   Token.{_type = Plus; value = "+"}         *)
(***********************************************)
let to_string = show_token
