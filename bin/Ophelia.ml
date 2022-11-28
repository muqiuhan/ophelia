let rec repl : unit -> unit =
 fun () ->
  let text = print_string "ophelia> " |> read_line in
  let interpreter = Ophelia_lib.Interpreter.init text in
  Ophelia_lib.Interpreter.expr interpreter |> print_int |> print_newline |> repl
;;

let () =
  let info =
    let tm = Unix.localtime (Unix.time ()) in
    Format.sprintf
      "
          Ophelia v0.0.1 (build with OCaml v%s) at %d-%02d-%02d %02d:%02d:%02d   
      "
      Sys.ocaml_version
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
  in
  print_endline info;
  repl ()
;;
