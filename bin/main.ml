open Frontend
open Semantic
open Llvm_ir_generator

(* 词法和语法分析 *)
let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let result = Parser.prog Lexer.token lexbuf in
  close_in chan;
  result

let () =
  (* 配置logger *)
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  
  Logs.info (fun m -> m "Parsing");
  let prog = parse_file "test/manbo/test.manbo" in

  Logs.info (fun m -> m "Semantic checking");
  let _vsymtbl, _fsymtbl, tytbl = prog |> Check.check_prog in

  Logs.info (fun m -> m "Generating LLVM IR");
  let _ = Irgen.gen_prog prog tytbl in
  Irgen.dump ()

