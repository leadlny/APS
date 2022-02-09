open Ast

type valeur = 
    InZ of int
    | InF of sexpr * string list *(string* valeur) list
    | InFR of string * valeur
    | InA of int
    | InP of block * string list *(string * valeur) list
    | InPR of string * valeur
    | Any


let get_val e =
    match e with
    InZ(v) -> v
    | InA(a) -> a
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
let add = ref 0;;

let alloc memoire = add:= (!add+1) ; (!add,Any)::memoire;;

let new_env a b  env = List.append (List.map2 (fun x y -> (x,y)) a b ) env;;

let rec eval_prog p =
  match p with
      ASTProg(b) -> eval_block b [] [] 

and eval_block b env mem  =
    match b with
        ASTBlock(cs) -> let memoire = eval_cmds cs env mem in memoire

and eval_cmds  cmds env mem =
    match cmds with
        [] -> mem
        | c::cs -> (
            match c with
                ASTDef d -> let newEnv, newMem = eval_def d env mem in eval_cmds cs newEnv newMem
                | ASTStat s -> let mem2 = eval_stat s env mem in eval_cmds cs env mem2
        )
    
and eval_stat s env mem =
    match s with
    ASTEcho e -> Printf.printf"%d \n" (get_val(eval_sexpr e env mem));mem
    | ASTSet(s,e) -> (let x = List.assoc s env in if List.mem_assoc (get_val x) mem then
                            let newMem = List.remove_assoc (get_val x) mem in
                            (get_val x, eval_sexpr e env mem)::mem
                            else failwith" adresse pas en mémore")
    | ASTIfS(e, b1,b2) -> (
        let v =  get_val ( eval_sexpr e env mem)
            in if v==1  then eval_block b1 env mem else if v==0 then eval_block b2 env mem else failwith "erreur if"
    )
    | ASTWhile(e, b) -> (
        let v = get_val ( eval_sexpr e env mem)
        in if v == 0 then mem else if v == 1 then let newMem = eval_block b env mem in eval_stat s env newMem else failwith " erreur while"

    )
    | ASTCall(s, es) -> (
        let x = List.assoc s env and res_list = list_sexpr es env mem in
        match x with
        InP(b,a,envir) -> (
            let newEnv = new_env a res_list  envir in
            eval_block b newEnv mem

        )
        |InPR(id,InP(b,a,envir)) -> (
            let newEnv = new_env a res_list envir in
            eval_block b ((id, InPR(id, InP(b, a, envir )))::newEnv) mem

        )
        | _ -> failwith " erreur avec call"
    )

and eval_def d env mem =
    match d with
    ASTConst(s, t, e)-> (s, ( eval_sexpr e env mem))::env , mem
    | ASTFun(s, t, args, e) -> (s, InF(e, eval_args args, env))::env , mem
    | ASTFunRec(s, t, args, e) ->(s,InFR(s,InF(e,eval_args args, env)))::env , mem
    | ASTVar (s, t) -> let newMem = alloc mem in (s,InA(!add))::env , newMem
    | ASTProc(s, args, b)->(s,InP(b,eval_args args, env))::env, mem
    | ASTProcRec(s, args, b)->(s,InPR(s,InP(b, eval_args args,env)))::env, mem

and eval_args args =
    match args with
    []->[]
    | a::ars ->(
        match a with
        ASTArg (s,t)->s::eval_args ars
    )
and list_sexpr exprs env mem = 
	match exprs with
	|e::[]-> (eval_sexpr e env mem)::[]
	|e::es->(eval_sexpr e env mem)::(list_sexpr es env mem) 



and eval_sexpr e env mem =
    match e with
        ASTNum n -> InZ(n)
        |ASTId id -> (
            let v = List.assoc id env in
            match v with
            InA(a) -> List.assoc a mem
            | _-> v

        )
        |ASTBool b -> if b == true then InZ(1) else InZ(0)
        |ASTApp (e, es) -> (
            let res_eval = eval_sexpr e env mem and res_list = list_sexpr es env mem in
            match res_eval with
                InF(e, a, envir) ->(
                    let newEnv= new_env a res_list envir in
                    eval_sexpr e newEnv mem
                )
                | InFR(id, InF(e, a ,envir))->(
                    let newEnv = new_env a res_list  envir in
                    eval_sexpr e ((id, InFR(id, InF(e, a, envir )))::newEnv) mem
                )
        )
                 

        |ASTPrim (s,eg,ed)->(
            let e1= eval_sexpr eg env mem in
            let e2= eval_sexpr ed env mem in
            eval_prim s e1 e2
        )
        |ASTUnaire (s, e) -> if (get_val (eval_sexpr e env mem) == 0) then InZ(1) else InZ(0)
        |ASTIf(s, e1,e2,e3) ->(
            let v1 =  get_val ( eval_sexpr e1 env mem)
            in if v1==0 then eval_sexpr e3 env mem else eval_sexpr e2 env mem
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