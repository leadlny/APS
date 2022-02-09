%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT PRIM NOT IF
%token LPAR RPAR TRUE FALSE BOOL INT STAR COMMA DDOTS ARROW ECHO FUN REC LBRACK RBRACK CONST SEMICOL

%type <Ast.sexpr> sexpr
%type <Ast.sexpr list> sexprs
%type <Ast.typeAPS> typeAPS
%type <Ast.typeAPS list> typesAPS
%type <Ast.stat> stat
%type <Ast.arg> arg
%type <Ast.arg list> args
%type <Ast.def> def
%type <Ast.cmd list> cmds
%type <Ast.prog> prog

%start prog             /* the entry point */

%%

  sexpr :
    NUM                                 { ASTNum($1) }
  | IDENT                               { ASTId($1) }
  | TRUE                                { ASTBool(true)}
  | FALSE                               { ASTBool(false)}
  | LPAR PRIM sexpr sexpr RPAR          { ASTPrim($2,$3,$4) }
  | LPAR NOT sexpr RPAR                 { ASTUnaire($2,$3) }
  | LPAR IF sexpr sexpr sexpr RPAR      { ASTIf($2,$3,$4,$5)}
  | LPAR sexpr sexprs RPAR              { ASTApp($2, $3) }
	| LBRACK args RBRACK sexpr            {ASTAbs($2,$4)}

  ;
  sexprs :
    sexpr       { [$1] }
  | sexpr sexprs { $1::$2 }
  ;

  typeAPS : 
    BOOL                      {ASTPrimt(Ast.BOOL)}
  | INT                       {ASTPrimt(Ast.INT)}
  | LPAR typesAPS ARROW typeAPS RPAR {ASTTypeFun($2,$4)}
  ;

  typesAPS :
    typeAPS                   {[$1]}
  | typeAPS STAR typesAPS     { $1::$3 }
  ;

  stat :
    ECHO sexpr                {ASTEcho($2)}
  ;

  arg :
    IDENT DDOTS typeAPS       {ASTArg($1,$3)}
  ;
  args :
    arg                       {[$1]}
  | arg COMMA args            { $1::$3 }
  ;

  def :
    CONST IDENT typeAPS sexpr                        {ASTConst($2,$3,$4)}
  | FUN IDENT typeAPS LBRACK args RBRACK sexpr       {ASTFun($2,$3,$5,$7)}
  | FUN REC IDENT typeAPS LBRACK args RBRACK sexpr   {ASTFunRec($3,$4,$6,$8)}

  cmds :
    stat                      {[ASTStat($1)]}
  | def SEMICOL cmds               {ASTDef($1)::$3}
  | stat SEMICOL cmds              {ASTStat($1)::$3}

  prog :
    LBRACK cmds RBRACK        {ASTProg($2)}

