(********************************************************************)
(* Aspect JoCaml - Implementation                                   *)
(*                                                                  *)
(* main.ml                                                          *)
(********************************************************************)
(* Time-stamp: <05-08-2011 09:19 by Nicolas Tabareau> *)

open Common
open Syntax

let debug = gen_debug debug_main "ajocaml"

(* Main function *)
let compile chan =
  let rec parse_until_end accum = 
    try 
      let s = input_char chan in
      (parse_until_end  (accum^(String.make 1 s)))
    with End_of_file -> accum
  in
  let code = parse_until_end "" in
  let lexbuf = Lexing.from_string code in
  debug "Lexer built" ;
  let class_ast =
    (try 
       Parser.gen_code Lexer.token lexbuf
     with
         Common.Syntax_error (s,i) ->
	   (debug ("Syntax error: "^s^" "^(Common.info_to_string i));
            exit 1)
       | Common.Parse_error (s,i)  ->
	   (debug ("Parsing error: "^s^" "^(Common.info_to_string i)); 
            exit 2)
    ) in
  if Common.debug_parserlexer then
    debug "Parsing succeded";
  let s = translate class_ast in 
  s
        


        
let _ = 
  let options = function
    | "-v" -> ()
    | fn -> 
      let chan = open_in fn in
      debug ("Compiling \"" ^ fn ^ "\"") ;
      let parse_result = compile chan in
      begin 
	close_in chan; 
	let fn_out = String.sub fn 0 ((String.length fn) - 3) ^ "ml" in
	let chan_out = open_out fn_out in 
	begin
	  output_string chan_out parse_result ; 
	  close_out chan_out ; 
	  debug ("Output written in file \"" ^ fn_out ^ "\"");
	end
      end in
  let () = flush_all () in                      
  let () = 
    for i = 1 to Array.length Sys.argv -1 do
      options Sys.argv.(i) ;
    done
  in 
  ()
    
