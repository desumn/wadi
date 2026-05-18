type t = private {
  start : Lexing.position;
  end_ : Lexing.position;
  is_ghost : bool;
}

val make : Lexing.position * Lexing.position -> bool -> t
val from_lexbuf : Lexing.lexbuf -> bool -> t
val set_ghost : t -> t
val dummy : t
val pp : t Fmt.t
