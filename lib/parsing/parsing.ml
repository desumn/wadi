module Location = Location
module Ast = Ast

exception Parse_error of string * Location.t

module I = Parser.MenhirInterpreter

let succeed (ast : Ast.program) = ast

let fail (lexbuf : Lexing.lexbuf) (checkpoint : Ast.program I.checkpoint) =
  match checkpoint with
  | I.HandlingError env ->
      (* let state_num = I.current_state_number env in *)
      let msg =
        match (* Parser_messages.message state_num *) "syntax error" with
        | msg -> msg
        | exception Not_found -> "syntax error (no message found)"
      in
      raise (Parse_error (msg, Location.from_lexbuf lexbuf false))
  | _ -> assert false

let supplier (lexbuf : Lexing.lexbuf) () =
  let token = Lexer.token lexbuf in
  (token, lexbuf.lex_start_p, lexbuf.lex_curr_p)

let parse_lexbuf (lexbuf : Lexing.lexbuf) =
  let checkpoint = Parser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail lexbuf) (supplier lexbuf) checkpoint

let parse_string ?(name = "string") string =
  let lexbuf = Lexing.from_string string in
  Lexing.set_filename lexbuf name;
  parse_lexbuf lexbuf
