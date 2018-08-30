#load unix.cma

open Char
open List
open String

open Sys

let rmd2r_script : unit -> string =
  fun _ ->
    (* getcwd () ^ "/src/SimpleR/R/Parser/hack/rmd2rscript.R" *)
    "/home/celery/foo/harvard/simple-r/src/SimpleR/R/Parser/hack/rmd2rscript.R"

let rmd_tmp : unit -> string =
  fun _ ->
    (* getcwd () ^ "/~rmd.tmp" *)
    "/home/celery/foo/harvard/simple-r/~rmd.tmp"

let read_R_file : string -> string =
  fun r_file ->
    let _ = Unix.open_process "hey" in
    let cmd_str = "rm -rf " ^ rmd_tmp () ^ "&& " ^
                  "touch " ^ rmd_tmp () ^ " && " ^
                  "Rscript " ^ rmd2r_script () ^ " " ^ r_file ^
                              " > " ^ rmd_tmp () in
    let _ = command cmd_str in
    let lines = ref [] in
    let in_chan = open_in (rmd_tmp ()) in
      try
        while true; do lines := input_line in_chan :: !lines; done; "";
      with End_of_file ->
        let _ = close_in in_chan in
          String.concat "\n" (rev !lines)

let parse_R_file : string -> unit Rast.program =
  fun r_file ->
    let file_str = read_R_file r_file in
    let lexbuf = Lexing.from_string file_str in
    let absyn =
      try
        Parser.prog (Lexer.tokenize (ref [])) lexbuf
      with _ ->
        let pos = lexbuf.Lexing.lex_curr_p in
        begin
          failwith ("port: syntax error detected at line " ^
                    string_of_int pos.Lexing.pos_lnum ^ " column " ^
                    string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
        end in
      absyn

let main : unit -> unit =
  fun _ ->
    let args = Array.to_list Sys.argv in
    let r_file = match args with
                  | [] -> failwith "port: exactly one filename expected"
                  | (_ :: arg :: _) -> arg
                  | _ -> failwith "port: exactly one filename expected" in
    
    let absyn = parse_R_file r_file in
    let _ = print_endline (Rast.string_of_program absyn) in
      ();;

main ();

