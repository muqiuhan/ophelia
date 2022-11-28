open Core

type lexical =
  { text : string (* client string input *)
  ; pos : int (* index into test *)
  ; current_token : Token.t (* current token instance *)
  }
[@@deriving show]

type t = lexical

let error : unit -> 'a = fun () -> failwith "Error parsing input"

let init : string -> lexical =
  fun text ->
  { text; pos = 0; current_token = Token.none }

(***************************************************************************)
(* Lexical analyzer (also known as scanner or tokenizer)                   *)
(* This function is responsible for breaking a sentence apart into tokens. *)
(* One token at a time.                                                    *)
(***************************************************************************)
let get_next_token : lexical -> Token.t * lexical =
 fun { text; pos; current_token } ->
  (* is pos index past the end of the text? if so, then return EOF token. *)
  if pos > String.length text - 1
  then Token.{ token_type = EOF; value = "EOF" }, { text; pos; current_token }
  else (
    (* get a character at the position pos and decide what token create based on the single character. *)
    let current_char : char = String.get text pos in
    (* if the character is digit then convert it to integer,
       create an integer token and increment pos index to point to the next character after the digit.
       and return the integer token. *)
    if Char.is_digit current_char
    then (
      let current_token =
        Token.{ token_type = Integer; value = Char.escaped current_char }
      in
      current_token, { text; pos = pos + 1; current_token })
    else if Char.equal '+' current_char
    then (
      let current_token =
        Token.{ token_type = Plus; value = Char.escaped current_char }
      in
      current_token, { text; pos = pos + 1; current_token })
    else error ())
;;

(***********************************************************************************************)
(* compare the current token type with the passed token type and                               *)
(* if they match then "eat" the current token and assign the next token to the current_token,  *)
(* otherwise raise an exception.                                                               *)
(***********************************************************************************************)
let eat : lexical -> Token.Type.t -> Token.t * lexical =
 fun lex token_type ->
  if phys_equal lex.current_token.token_type token_type
  then get_next_token lex
  else error ()
;;

(**********************************************************)
(* expr -> INTEGER PLUS INTEGER                           *)
(**********************************************************)
let expr : lexical -> int =
 fun lex ->
  (* set current token to the first token taken from first. *)
  let current_token, lex = get_next_token lex in
  (* we except the current token to be a singal-digit integer *)
  let left = Int.of_string current_token.value in
  let current_token, lex = eat lex Token.Type.Integer in
  (* we except the current token to be a '+' token *)
  let _operator = current_token in
  let current_token, lex = eat lex Token.Type.Plus in
  (* we except the current token to be a singal-digit integer *)
  let right = Int.of_string current_token.value in
  let _current_token, _lex = eat lex Token.Type.Integer in
  (* at this point INTEGER PLUS INTEGER sequence of tokens has been successfully found
     and the method can just return the result of adding two integers,
     thus effectively interpreting client input *)
  left + right
;;
