(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type tprim = BOOL | INT
type cond = If

let string_of_bool bool=
  match bool with 
    true -> "true"
  | false -> "false"
  
let string_of_tprim t=
  match t with
    INT -> "int"
  | BOOL -> "bool"


type sexpr =
    ASTNum of int
  | ASTId of string
  | ASTBool of bool
  | ASTApp of sexpr * sexpr list
  | ASTPrim of string * sexpr * sexpr
  | ASTUnaire of string * sexpr
  | ASTIf of string * sexpr * sexpr * sexpr
  | ASTAbs of arg list * sexpr

	
and typeAPS = 
  ASTPrimt of tprim 
  | ASTTypeFun of typeAPS list * typeAPS

and stat =
  ASTEcho of sexpr

and arg =
  ASTArg of string * typeAPS

and def =
  ASTConst of string * typeAPS * sexpr
  |ASTFun of string * typeAPS * arg list * sexpr
  |ASTFunRec of string * typeAPS * arg list * sexpr

and cmd =
  |ASTDef of def 
  |ASTStat of stat 

and prog =
  ASTProg of cmd list
