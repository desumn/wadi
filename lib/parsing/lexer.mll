{
open Parser

exception Lexing_error of (string * Lexing.position)

}

let digit   = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter  = lowercase | uppercase
let lident   = lowercase (letter | digit | '_')*
let uident   = uppercase (letter | digit | '_')*
let number  = digit (digit)*

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { comment 1 lexbuf }
  | "//" { line_comment lexbuf }
  | "(" {ParenOpen}
  | ")" {ParenClose}
  | "," {Comma}
  | "=" {Equal}
  | "+" {Plus}
  | "-" {Minus}
  | "*" {Star}
  | "/" {Slash}
  | "|" {Bar}
  | "_" {Underscore}
  | "|>" {Pipe}
  | "<|" {RevPipe}
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
  | "match" {Match}
  | "with" {With}
  | "end" {End}
  | number as num {Int (int_of_string num)}
  | lident as ident {Lident ident}
  | uident as ident {Uident ident}
  | eof {Eof}
  | _ as c {raise (Lexing_error ("Not handled: " ^ String.make 1 c, Lexing.lexeme_start_p lexbuf))}
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf}
  | "*)" { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof { raise (Lexing_error ("eof in comment", Lexing.lexeme_start_p lexbuf)) }
  | _ { comment depth lexbuf }
and line_comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof  { Eof }
  | _    { line_comment lexbuf }
