{
(********************************************************************)
(* Aspect JoCaml - Implementation                                   *)
(*                                                                  *)
(* lexer.mll                                                          *)
(********************************************************************)
(* Time-stamp: <05-08-2011 09:19 by Nicolas Tabareau> *)

open Common
open Syntax
open Parser

module LE = Lexing

let debug = gen_debug Common.debug_parserlexer "lexer"

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1

let newline lexbuf : unit =
(*  debug "New line added" ; *)
  linestart := LE.lexeme_start lexbuf; 
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;} ;
  incr lineno

let info lexbuf : info =
(*  debug "Gathering lexeme information" ; *)
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
  (!lineno, c1 - !linestart),(!lineno, c2 - !linestart)

let error lexbuf s =
  debug ("Error generation: "^s) ;
  let (l1,c1),(l2,c2) = info lexbuf in
  let t = lexeme lexbuf in
  let s = Printf.sprintf " %d:%d-%d: %s" l1 c1 c2 s in
  if t = "" then (debug ("No lexeme found") ;raise (Syntax_error (s,((l1,c1),(l2,c2)))))
  else (debug ("Lexeme found: "^t) ;raise (Syntax_error (s^ ": " ^ t,((l1,c1),(l2,c2)))))

}

let blank = [' ' '\t']+
let symbol = [^ '"' '=' '{' '}' '[' ']' '(' ')' ':'
                  '<' '>' ';' ',' '\n' '\r' ' ' '\t' '-' '+' '#' '$']+
let identlower = [ 'a' - 'z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*
let identupper = [ 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*
let identifier = [ 'a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*
let ocamltype = [ 'a' - 'z' 'A' - 'Z' '\'' '_'][ 'a' - 'z' 'A' - 'Z' '0' - '9' '.' ]*
let channel = "spawn " [ 'a' - 'z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '#']* '('
let emptychannel = "spawn " [ 'a' - 'z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '#']* "()"
let nat = ['0' - '9']+
let newline = '\n' 
let char = [^ '\n']
let string = ['"'] identlower ['"']

rule token = parse
  | blank         { token lexbuf }
  | "class"     { CLASS (info lexbuf) }
  | "aspect"     { ASPECT (info lexbuf) }
  | "pc"     { PC (info lexbuf) }
  | "PCTrue"         { TRUEPC (info lexbuf) }
  | "PCFalse"         { FALSEPC (info lexbuf) }
  | "Contains"     { CONTAINSPC (info lexbuf) }
  | "&&&"         { ANDPC (info lexbuf) }
  | "Not"         { NOTPC (info lexbuf) }
  | "Host"         { HOSTPC (info lexbuf) }
  | "IsHost"         { ISHOSTPC (info lexbuf) }
  | "CausedBy"         { CAUSEDBYPC (info lexbuf) }
  | "advice"     { ADVICE (info lexbuf) }
  | "proceed"     { PROCEED (info lexbuf) }
  | "end"     { END (info lexbuf) }
  | "dist_object"     { D_OBJECT (info lexbuf) }
  | "at"      { AT (info lexbuf) }
  | "reaction"      { REACTION (info lexbuf) }
  | "'opaque"      { OPAQUE (info lexbuf) }
  | "'advisable"      { ADVISABLE (info lexbuf) }
  | "'synchronous"      { SYNCHRONOUS (info lexbuf) }
  | "'observable"      { OBSERVABLE (info lexbuf) }
  | "private"      { PRIVATE (info lexbuf) }
  | "public"      { PUBLIC (info lexbuf) }
  | "channel"      { CHANNEL (info lexbuf) }
  | "{"       { let s = code lexbuf in CODE (info lexbuf, s) }
  | "="           { EQUAL (info lexbuf) }
  | "->"          { ARROW (info lexbuf) }
  | ":"           { COLON (info lexbuf) }
  | ";"           { SEMI (info lexbuf) }
  | "."           { DOT (info lexbuf) }
  | ","           { COMMA (info lexbuf) }
  | "("           { LPA (info lexbuf) }
  | ")"           { RPA (info lexbuf) }
  | "["           { LBR (info lexbuf) }
  | "]"           { RBR (info lexbuf) }
  | "+"           { PLUS (info lexbuf) }
  | "||"          { PAR (info lexbuf) }
  | "|"           { ORR (info lexbuf) }
  | "*"           { MUL (info lexbuf) }
  | "<"           { INF (info lexbuf) }
  | "or"      { OR (info lexbuf) }
  | "&"      { WITH (info lexbuf) }
  | "#"      { SHARP (info lexbuf) }
  | identupper    { IDENTUP (info lexbuf,lexeme lexbuf) }
  | identlower    { IDENTLO (info lexbuf,lexeme lexbuf) }
  | string        { STRING (info lexbuf,lexeme lexbuf) }
  | "*"           { WILDCARD (info lexbuf) }
  | ocamltype     { TYP (info lexbuf,lexeme lexbuf) }
  | '1'           { ONE (info lexbuf) }
  | '0'           { ZERO (info lexbuf) }
  | eof		  { EOF (info lexbuf) }
  | "\r\n"        { newline lexbuf; token lexbuf }
  | newline       { newline lexbuf; token lexbuf }
  | "(*"          { comment lexbuf; token lexbuf }
  | _		  { error lexbuf "Unknown token" }

and comment = parse
  | "(*"             { comment lexbuf; comment lexbuf }
  | "*)"             { () }
  | newline          { newline lexbuf; comment lexbuf }
  | eof		     { error lexbuf "Unmatched '(*'" }
  | _                { comment lexbuf }

and code  = parse
  | newline          { newline lexbuf; "\n" ^ code lexbuf }     
  | "}"             { "" }
  | eof		     { error lexbuf "Unmatched '(end'" }
  | emptychannel as x    { (String.sub x 0 ((String.length x) - 1)) ^ "Join_point.EmptyFlow)" ^ (code lexbuf) }
  | channel as x         { (String.sub x 0 ((String.length x) - 0)) ^ "Join_point.EmptyFlow," ^ (code lexbuf) }  
  | _ as x           { (String.make 1 x)^(code lexbuf) }
