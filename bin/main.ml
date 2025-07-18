open Manbo_compiler

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let result = Parser.prog Lexer.token lexbuf in
  close_in chan;
  result

let () =
  let prog = parse_file "test/manbo/test.manbo" in
  prog |> Ast.show_prog |> print_endline
