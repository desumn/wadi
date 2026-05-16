module Location = Location
module Ast = Ast

exception Parse_error of string * (Location.t)

val parse_lexbuf : Lexing.lexbuf -> Ast.expr

val parse_string : ?name:string -> string -> Ast.expr
