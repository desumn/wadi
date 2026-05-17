module Location = Location
module Ast = Ast

exception Parse_error of string * Location.t

val parse_lexbuf : Lexing.lexbuf -> Ast.program
val parse_string : ?name:string -> string -> Ast.program
