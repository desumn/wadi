type t = { start : Lexing.position; end_ : Lexing.position; is_ghost : bool }

let make (start, end_) is_ghost = { start; end_; is_ghost }

let from_lexbuf (lexbuf : Lexing.lexbuf) =
  make (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let dummy =
  { start = Lexing.dummy_pos; end_ = Lexing.dummy_pos; is_ghost = true }

let set_ghost loc = { loc with is_ghost = true }

let pp ppf loc =
  Fmt.pf ppf "%s: %d:%d-%d:%d" loc.start.pos_fname loc.start.pos_lnum
    (loc.start.pos_cnum - loc.start.pos_bol)
    loc.end_.pos_lnum
    (loc.end_.pos_cnum - loc.end_.pos_bol)
