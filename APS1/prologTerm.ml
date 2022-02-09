(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
  
let rec print_sexpr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTBool x -> Printf.printf"bool(%s)" (string_of_bool x)
    | ASTApp(e, es) -> (
	      Printf.printf"app(";
      	print_sexpr e;
	      Printf.printf",[";
	      print_sexprs es;
	      Printf.printf"])"
        )
    |ASTPrim(op, e1, e2) ->(
        Printf.printf"%s(" op;
        print_sexpr e1;
        Printf.printf",";
        print_sexpr e2;
        Printf.printf")"
        )
    |ASTUnaire(op,e) ->(
      Printf.printf"%s(" op;
      print_sexpr e;
      Printf.printf")"
    )
    |ASTIf(op,e1,e2,e3) ->(
      Printf.printf"%s(" op;
      print_sexpr e1;
      Printf.printf",";
      print_sexpr e2;
      Printf.printf",";
      print_sexpr e3;
      Printf.printf")"
    )

    |ASTAbs(args,e) -> (
      Printf.printf"abs([";
      print_args args;
      Printf.printf"],";
      print_sexpr e;
      Printf.printf")"
    )

and print_sexprs es =
  match es with
      [] -> ()
    | [e] -> print_sexpr e
    | e::es -> (
	print_sexpr e;
	print_char ',';
	print_sexprs es
      )

and print_typeAPS t = 
  match t with
    ASTPrimt t -> Printf.printf"%s" (string_of_tprim t)
  | ASTTypeFun(ts,t) -> (
      Printf.printf"typefun([";
      print_typesAPS ts;
      Printf.printf"],";
      print_typeAPS t;
      Printf.printf")"
  )

and print_typesAPS ts =
	match ts with
	  []->()
	  | [t] -> print_typeAPS t
  	| t::ts -> (
	  	print_typeAPS t;
	  	Printf.printf ",";
	  	print_typesAPS ts;
		)
and print_block b =
  match b with
    ASTBlock cs -> (
      Printf.printf"block([";
      print_cmds cs;
      Printf.printf"])";
    )

and print_stat s =
  match s with
    ASTEcho expr -> (
      Printf.printf"echo(";
      print_sexpr expr;
      Printf.printf")"
    )
   | ASTSet(id, expr) ->(
      Printf.printf"set(%s," id;
      print_sexpr expr;
      Printf.printf")"
   )
   | ASTIfS(expr, b1,b2) ->(
      Printf.printf"ifStat(";
      print_sexpr expr;
      Printf.printf ",";
      print_block b1;
      Printf.printf ",";
      print_block b2;
      Printf.printf")"
   )
   | ASTWhile(expr,b) ->(
     Printf.printf"while(";
     print_sexpr expr;
     Printf.printf ",";
     print_block b;
     Printf.printf ")";
   )
   | ASTCall(id, exprs) -> (
     Printf.printf"call(%s," id;
     Printf.printf"[";
     print_sexprs exprs;
     Printf.printf"]";
     Printf.printf ")";
     
   )

and print_arg a =
  match a with
    ASTArg (id,t) ->(
      Printf.printf"(%s," id;
      print_typeAPS t;
      Printf.printf")"
    )
and print_args ars =
	match ars with
	  []->()
	  | [a] -> print_arg a
  	| a::ars -> (
	  	print_arg a;
	  	Printf.printf ",";
	  	print_args ars;
		)
and print_def d =
  match d with
    ASTConst (id,t,e) ->(
      Printf.printf"defconst(%s," id;
      print_typeAPS t;
      Printf.printf",";
      print_sexpr e;
      Printf.printf")"
    )
    | ASTFun (id, t, ars, e) -> (
        Printf.printf "deffun(%s," id ;
        print_typeAPS t ;
        print_string ",[" ;
        print_args ars ;
        print_string "]," ;
        print_sexpr e ;
        print_string ")"
      )
    | ASTFunRec (id, t, ars, e) -> (
        Printf.printf "deffunrec(%s," id ;
        print_typeAPS t ;
        print_string ",[" ;
        print_args ars ;
        print_string "]," ;
        print_sexpr e ;
        print_string ")"
      )
    | ASTVar (id,t) -> (
      Printf.printf"var(%s," id;
      print_typeAPS t;
      Printf.printf ")";
    )
    | ASTProc (id, ars, b) -> (
        Printf.printf "proc(%s," id ;
        print_string "[" ;
        print_args ars ;
        print_string "]," ;
        print_block b ;
        print_string ")";
    )
    |ASTProcRec (id, ars, b) -> (
        Printf.printf "procRec(%s," id ;
        print_string "[" ;
        print_args ars ;
        print_string "]," ;
        print_block b ;
        print_string ")";

    )
and print_cmd c =
  match c with 
    ASTDef d -> print_def d
    |ASTStat s -> print_stat s

and print_cmds cs =
    match cs with
    | [] -> ()
    | [c] -> print_cmd c
    | c::cs -> (
        print_cmd c;
        print_string ",\n";
        print_cmds cs
      )

and print_prog p = 
  match p with
    | ASTProg b -> (
        print_string "prog(\n";
        print_block b;
        print_string "\n)." 
      )

;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let e = Parser.prog Lexer.token lexbuf in
      print_prog e;
  with Lexer.Eof ->
    exit 0
      
