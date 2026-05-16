open Wadi
open Cmdliner

let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let parse_file path =
  let open Bos in
  let open Result.Syntax in
  let* file = OS.File.must_exist path in
  let* content = OS.File.read file in
  let lexbuf = Lexing.from_string content in
  Lexing.set_filename lexbuf (Fpath.to_string file);
  match Parsing.parse_lexbuf lexbuf with
  | ast -> Ok ast
  | exception Parsing.Parse_error (error, _) -> Error (`Msg error)

let program_arg =
  let doc = "path to a wadi program" in
  Arg.(required & pos 0 (some fpath) None & info [] ~docv:"SOURCEFILE" ~doc)

let parse_and_execute file =
  match parse_file file with
  | Ok ast ->
      begin match Eval.eval_expr Eval.Env.empty ast with
      | Ok i ->
          Fmt.pr "=> %a@." Eval.pp_value i;
          Cmd.Exit.ok
      | Error err ->
          Fmt.epr "error: %a@." Eval.pp_error err;
          Cmd.Exit.some_error
      end
  | Error (`Msg error_message) ->
      Fmt.epr "%s" error_message;
      Cmd.Exit.some_error

let cmd =
  let doc = "Wadi reference interpreter" in
  let info = Cmd.info "wadi" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const parse_and_execute $ program_arg)

let _ = exit (Cmd.eval' cmd)
