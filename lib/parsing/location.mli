type t = private { start : Lexing.position; end_ : Lexing.position }

val make : Lexing.position * Lexing.position -> t
val from_lexbuf : Lexing.lexbuf -> t
val pp : t Fmt.t
