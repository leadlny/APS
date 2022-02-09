open Ast

type valeur = 
    InZ of int
    | InF of sexpr * string list *(string* valeur) list
    | InFR of string * valeur


let get_val e =
    match e with
    InZ(v) -> v
;;
let eval_prim op e1 e2 =
    match op with
        "add" -> InZ((get_val e1) + get_val e2)
        |"sub" -> InZ((get_val e1) - get_val e2)
        |"mul" -> InZ((get_val e1) * get_val e2)
        |"div" -> InZ((get_val e1) / get_val e2)
        |"eq" -> InZ( if (get_val e1) == (get_val e2) then 1 else 0)
        |"lt" -> InZ( if (get_val e1) < (get_val e2) then 1 else 0)
        |"and" -> if (get_val e1 ==0) then InZ(0) else  InZ(get_val e2)
        |"or" -> if (get_val e1 ==1) then InZ(1) else InZ(get_val e2)
;;


let rec eval_prog p =
  match p with
      ASTProg(cs) -> eval_cmds cs []

and eval_cmds  cmds env  =
    match cmds with
        [] -> ()
        | c::cs -> (
            match c with
                ASTDef d -> let newEnv = eval_def d env  in eval_cmds cs newEnv 
                | ASTStat s -> eval_stat s env ; eval_cmds cs env 
        )
    
and eval_stat s env  =
    match s with
    ASTEcho e -> Printf.printf"%d \n" (get_val(eval_sexpr e env ))

and eval_def d env  =
    match d with
    ASTConst(id, t, e)-> (id, ( eval_sexpr e env))::env
    | ASTFun(id, t, args, e) -> (id, InF(e, eval_args args, env))::env
    | ASTFunRec(id, t, args, e) ->(id,InFR(id,InF(e,eval_args args, env)))::env

and eval_args args =
    match args with
    []->[]
    | a::ars ->(
        match a with
        ASTArg (s,t)->s::eval_args ars
    )
and list_sexpr exprs env = 
	match exprs with
	|e::[]-> (eval_sexpr e env)::[]
	|e::es->(eval_sexpr e env)::(list_sexpr es env)

and new_env a b  env = List.append (List.map2 (fun x y -> (x,y)) a b ) env

and eval_sexpr e env =
    match e with
        ASTNum n -> InZ(n)
        |ASTId id -> List.assoc id env
        |ASTBool b -> if b == true then InZ(1) else InZ(0)
        |ASTApp (e, es) -> (
            let res_eval = eval_sexpr e env and res_list = list_sexpr es env in
            match res_eval with
                InF(expr, a, envir) ->(
                    let newEnv= new_env a res_list envir in
                    eval_sexpr expr newEnv
                )
                | InFR(id, InF(expr, a ,envir))->(
                    let newEnv = new_env a res_list  envir in
                    eval_sexpr expr ((id, InFR(id, InF(expr, a, envir )))::newEnv) 
                )
        )
                

        |ASTPrim (s,eg,ed)->(
            let e1= eval_sexpr eg env in
            let e2= eval_sexpr ed env in
            eval_prim s e1 e2
        )
        |ASTUnaire (s, e) -> if (get_val (eval_sexpr e env) == 0) then InZ(1) else InZ(0)
        |ASTIf(s, e1,e2,e3) ->(
            let v1 =  get_val ( eval_sexpr e1 env)
            in if v1==0 then eval_sexpr e3 env else eval_sexpr e2 env
        )
        |ASTAbs(args, e) -> InF(e,eval_args args, env)
        
;;

let fname = Sys.argv.(1) in
  let ic = open_in fname in
    try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p;
      print_char '\n'
    with Lexer.Eof ->
    exit 0