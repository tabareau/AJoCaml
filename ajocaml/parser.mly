%{
  (********************************************************************)
  (* Aspect JoCaml - Implementation                                   *)
  (*                                                                  *)
  (* parser.mly                                                          *)
  (********************************************************************)
  (* Time-stamp: <05-08-2011 09:19 by Nicolas Tabareau> *)
  
  open Common
  open Syntax
  open Lexing

  let debug = gen_debug debug_parserlexer "parser"
 
  let error t info =
    debug ("Error: "^t) ;
    let (l,c1),(_,c2) = info in
    let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Common.Parse_error (s,info))
    else raise (Common.Parse_error ((*s^ ": " ^ *)t,info))

  let parse_error _ =
    let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    let (l1,c1),(l2,c2) =
      (start_pos.pos_lnum,start_pos.pos_bol),(end_pos.pos_lnum,end_pos.pos_bol)
    in
    raise (Common.Parse_error ("",((l1,c1),(l2,c2))))

    
    

%}

%token <Common.info> CLASS ASPECT PC ADVICE END D_OBJECT AT PROCEED
%token <Common.info> REACTION OPAQUE ADVISABLE SYNCHRONOUS OBSERVABLE
%token <Common.info> PRIVATE PUBLIC CHANNEL
%token <Common.info> EOF
%token <Common.info> LPA RPA LBR RBR
%token <Common.info> LANG
%token <Common.info * string> RANG
%token <Common.info> WILDCARD ANDPC NOTPC CONTAINSPC CAUSEDBYPC ISHOSTPC HOSTPC TRUEPC FALSEPC
%token <Common.info> DOT SEMI EQUAL COMMA MUL COLON PLUS ONE ZERO PAR OR ORR WITH SHARP ARROW INF 
%token <Common.info * string> IDENTUP IDENTLO STRING
%token <Common.info * string> TYP 
%token <Common.info * string> CODE
%left OR
%start gen_code
%type <Syntax.ast> gen_code

%%

gen_code: 
| CODE gen_code { [(emptyClass,emptyAspect,snd $1)] @ $2  }
| d_class gen_code { ( [$1 , emptyAspect, ""] @ $2)  }
| d_aspect gen_code { [(emptyClass,$1,"")] @ $2  }
|  { [] }

d_class: 
| CLASS polytype IDENTLO args_class EQUAL CODE d_object END { Dclass (snd $3, $2, $4 , snd $6, $7) }
| CLASS polytype IDENTLO args_class EQUAL d_object END { Dclass (snd $3, $2, $4 , "", $6) }
| IDENTLO                               { error "Classes should start with keyword 'class'" (fst $1) }
| IDENTUP                               { error "Classes should start with keyword 'class'" (fst $1) }
|                                       { error "Shouldn't happen" bogusInfo }
;

d_aspect: 
| ASPECT IDENTLO args_class EQUAL PC COLON d_pointcut ADVICE COLON CODE chem_react_right END { Daspect (snd $2, $3 , $7, snd $10,$11) }
| ASPECT IDENTLO args_class EQUAL PC COLON d_pointcut ADVICE COLON chem_react_right END { Daspect (snd $2, $3 , $7,"",$10) }
;

d_pointcut:
| CONTAINSPC IDENTLO DOT args_contains {Contains_ (snd $2,$4)}
| HOSTPC IDENTLO {Host_ (snd $2)}
| ISHOSTPC IDENTLO {IsHost_ (snd $2)}
| TRUEPC {PCTrue_}
| FALSEPC {PCFalse_}
| LPA d_pointcut RPA ANDPC LPA d_pointcut RPA {And_ ($2,$6)}
| NOTPC LPA d_pointcut RPA  {Not_ $3}
| CAUSEDBYPC LPA d_pointcut RPA {CausedBy_ $3}

args_contains:
| LBR pattern RBR { $2 }
;

pattern:
| STRING LPA bvariable RPA SEMI pattern { [(snd $1,$3)] @ $6 }
| STRING LPA bvariable RPA { [(snd $1,$3)] }
;

bvariable:
| IDENTLO COMMA bvariable { [snd $1] @ $3 }
| IDENTLO { [snd $1] }
;

polytype:
| LBR args_type RBR { $2 }
|   { [] }

args_type:
| TYP COMMA args_type { [snd $1] @ $3 }
| TYP { [snd $1] }
;

args_class:
| IDENTLO args_class { [(snd $1,"")] @ $2 }
| LPA IDENTLO COLON TYP RPA args_class { [(snd $2,snd $4)] @ $6 }
| { [] }
;

d_object:
| D_OBJECT self REACTION react_rule private_chan public_chan CODE 
    { Dobject ( $2, $4, $5 , $6, snd $7) }
| D_OBJECT self REACTION react_rule private_chan public_chan
    { Dobject ( $2, $4, $5 , $6, "") }
| D_OBJECT self REACTION react_rule public_chan CODE 
    { Dobject ( $2, $4, [] , $5, snd $6) }
| D_OBJECT self REACTION react_rule private_chan CODE 
    { Dobject ( $2, $4, $5 , [], snd $6) }
| D_OBJECT self REACTION react_rule public_chan
    { Dobject ( $2, $4, [] , $5, "") }
| D_OBJECT self REACTION react_rule private_chan
    { Dobject ( $2, $4, $5 , [], "") }
| D_OBJECT self REACTION react_rule 
    { error "no channel declared" ($1) }
| D_OBJECT self  
    { error "reaction missing" ($1) }
| D_OBJECT self REACTION
    { error "reaction missing" ($1) }
| D_OBJECT self REACTION private_chan public_chan CODE 
    { error "reaction missing" ($1) }
| D_OBJECT                               { error "Missing object identifier" ($1) }
| IDENTLO                               { error "Objects should start with keyword 'dist_object'" (fst $1) }
| IDENTUP                               { error "Objects should start with keyword 'dist_object'" (fst $1) }
|                                       { error "Shouldn't happen" bogusInfo }
;

self:
| LPA IDENTLO RPA {snd $2}
;

react_rule:
| IDENTLO AT IDENTLO COLON react_kind chem_react OR react_rule {[(snd $1,snd $3,$5,fst $6, snd $6)]@$8}
| IDENTLO AT IDENTLO COLON react_kind chem_react {[(snd $1,snd $3,$5,fst $6, snd $6)]}
| IDENTLO AT IDENTLO COLON {error "foo" bogusInfo}
| IDENTLO {error "foo" bogusInfo}
;

react_kind:
| OPAQUE { Opaque }
| ADVISABLE { Advisable }
| SYNCHRONOUS { Synchronous }
| OBSERVABLE { Observable }
;

chem_react:
| chem_react_left EQUAL chem_react_right { ($1,("",$3)) }
| chem_react_left EQUAL CODE chem_react_right { ($1,(snd $3,$4)) }
;

chem_react_left:
| channel WITH chem_react_left { [$1] @ $3 }
| channel { [$1] }
;

chem_react_right:
| channel WITH chem_react_right { [$1] @ $3 }
| ZERO WITH chem_react_right { $3 }
| channel { [$1] }
| ZERO { [] }
;

channel:
| PROCEED LPA RPA { ("proceed",["jp"]) }
| PROCEED LPA args RPA { error "proceed has no argument" ($1)}
| IDENTLO LPA args RPA { (snd $1,$3) }
| IDENTLO SHARP IDENTLO LPA args RPA { (snd $1 ^ "#" ^ snd $3,$5) }
;

args:
| IDENTLO COMMA args { [snd $1] @ $3 }
| CODE COMMA args { [snd $1] @ $3 }
| IDENTLO { [snd $1] }
| CODE { [snd $1] }
| { [] }
;

private_chan:
| PRIVATE CHANNEL chan_name { $3 }
;

public_chan:
| PUBLIC CHANNEL chan_name { $3 }
;

chan_name:
| LPA IDENTLO COLON gen_typ RPA COMMA chan_name { [(snd $2,$4)] @ $7 }
| LPA IDENTLO COLON gen_typ RPA { [(snd $2,$4)] }
| IDENTLO COMMA chan_name { [(snd $1,("",""))] @ $3 }
| IDENTLO { [(snd $1,("",""))] }
;

gen_typ: 
| CODE TYP { (snd $1,snd $2) }
| TYP TYP { (snd $1,snd $2) }
