open Frontend
open Semantic

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let result = Parser.prog Lexer.token lexbuf in
  close_in chan;
  result

let () =
  let prog = parse_file "test/manbo/test.manbo" in
  let vsymtbl, fsymtbl = prog |> Check.check_prog in
  print_endline "========== vsymtbl ==========";
  Sym_table.print vsymtbl 0;
  print_endline "========== fsymtbl ==========";
  Sym_table.print fsymtbl 0
