{
open Parser

exception Lexing_error of (string * Lexing.position)

}

let digit   = ['0'-'9']
let letter  = ['a'-'z' 'A'-'Z']
let ident   = letter (letter | digit | '_')*
let number = digit (digit)*

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { comment 1 lexbuf }
  | "(" {ParenOpen}
  | ")" {ParenClose}
  | "=" {Equal}
  | "+" {Plus}
  | "-" {Minus}
  | "*" {Star}
  | "/" {Slash}
  | "->" {Arrow}
  | "<>" {NotEqual}
  | "<" {Less}
  | "<=" {LessEqual}
  | ">" {Greater}
  | ">=" {GreaterEqual}
  | "andalso" {And}
  | "orelse" {Or}
  | "let" {Let}
  | "rec" {Rec}
  | "in" {In}
  | "fun" {Fun}
  | "true" {Bool true}
  | "false" {Bool false}
  | "if" {If}
  | "then" {Then}
  | "else" {Else}
  | number as num {Int (int_of_string num)}
  | ident as ident {Ident ident}
  | eof {Eof}
  | _ as c {raise (Lexing_error ("Not handled: " ^ String.make 1 c, Lexing.lexeme_start_p lexbuf))}
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf}
  | "*)" { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof { raise (Lexing_error ("eof in comment", Lexing.lexeme_start_p lexbuf)) }
  | _ { comment depth lexbuf }
