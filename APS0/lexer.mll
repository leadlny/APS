(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '('      { LPAR }
  | ')'      { RPAR }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | eof         { raise Eof }
  | "ECHO"      {ECHO}
  | "FUN"       { FUN }
  | "CONST"      { CONST }
  | "REC"       { REC }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "bool"      { BOOL }
  | "int"       { INT }
  | "*"         { STAR }
  | "->"        { ARROW }
  | ":"         { DDOTS }
  | ","         { COMMA }
  | ";"         { SEMICOL }
  | "not"   as lxm { NOT(lxm) }
  | "if"    as  lxm { IF(lxm) }
  | "add"|"eq"|"lt"|"and"|"sub"|"mul"|"div"|"or" as lxm { PRIM(lxm) }  
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
