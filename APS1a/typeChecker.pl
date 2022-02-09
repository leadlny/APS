
/* clauses qui vont nous servir 
tout au long de typeChecker
*/
%% assoc
assoc(X,[(X,V)|_],V).
assoc(X,[_|XVs],V) :- assoc(X,XVs,V).

%avec une liste d'arg,permet d'avoir une liste de type
getTypesArgs([(_,T)], [T]).
getTypesArgs([(vart(X), T1)|ARGS], [(ref(T1))|TL]) :- getTypesArgs(ARGS, TL).
getTypesArgs([(X, T1)|ARGS], [T1|TL]) :- getTypesArgs(ARGS, TL).

% Nous sert a avoir x,ti' ou donc ti ou ref ti
getA([(_,T)], [T]).
getA([(vart(X), T1)|ARGS], [(X,ref(T1))|TL]) :- getA(ARGS, TL).
getA([(X, T1)|ARGS], [(X,T1)|TL]) :- getA(ARGS, TL).

% nous sert à trouver le type de chaque expression dans une liste d'expression
%pour le mettre dans le dernier argument
typesliste(_, [], []).
typesliste(G, [E1|Er], [T1|Tr]) :- expr( E1,G, T1), typesliste(G, Er, Tr).

typesliste2(_, [], []).
typesliste2(G, [E1|Er], [T1|Tr]) :- expar( E1,G, T1), typesliste2(G, Er, Tr).

% expr(predicat,contexte , type)

expr(bool(true), _, bool).
expr(bool(false), _, bool).
expr(num(I), _, int) :- integer(I).
expr(id(I),G,T) :- assoc(I,G,T).
expr(id(I),G,T):- assoc(I,G,ref(T)).
expar(adr(I),G,ref(T)) :-assoc(I,G,ref(T)).
expar(I,G,T):-expr(I,G,T).

% operations qui renvoient des int
expr(add(E1,E2),G,int):-expr(E1,G,int),expr(E2,G,int).
expr(mul(E1,E2),G,int):-expr(E1,G,int),expr(E2,G,int).
expr(sub(E1,E2),G,int):-expr(E1,G,int),expr(E2,G,int).
expr(div(E1,E2),G,int):-expr(E1,G,int),expr(E2,G,int).
%operations qui renvoient des bool
expr(eq(E1,E2),G,bool):-expr(E1,G,int),expr(E2,G,int).
expr(lt(E1,E2),G,bool):-expr(E1,G,int),expr(E2,G,int).
expr(not(E1),G,bool):-expr(E1,G,bool).
expr(and(E1,E2),G,bool):-expr(E1,G,bool),expr(E2,G,bool).
expr(or(E1,E2),G,bool):-expr(E1,G,bool),expr(E2,G,bool).
%IF
expr(if(E1,E2,E3),G,T):-expr(E1,G,bool),expr(E2,G,T),expr(E3,G,T).
%ABS ()
expr(abs(Ar, E),G, typefun(Ts, T)) :- getTypesArgs(Ar, Ts), append(Ar, G, ArG), expr(E, ArG, T).
%APP ( On verifie le type de tous les expr dans ES et on met tout dans Ts, puis on verifie E)
expr(app(E, Es),G, T) :- typesliste(G, Es, Ts), expr(E, G, typefun(Ts, T)). 


%def(predicat,contexte, nouveau contexte)

def(defconst(Nom,T,E),G,[(Nom,T)|G]):- expr(E,G,T).
def(deffun(Nom,T,Ar,E),G,[(Nom,typefun(Ts,T))|G]):- getTypesArgs(Ar,Ts),append(Ar,G,ArG),expr(E,ArG,T).%On remplie Ts avec TypesArgs, Ensuite on ajoute nos arguments au contexte, puis on test l'expression avec le nouveau context
def(deffunrec(Nom,T,Ar,E),G,[(Nom,typefun(Ts,T))|G]):-getTypesArgs(Ar,Ts),append(Ar,G,ArG),expr(E,[(Nom,typefun(Ts,T))|ArG],T).
def(var(Nom,T),G,[(Nom,ref(T))|G]).
def(proc(Nom,Ar,B),G,[(Nom,typefun(Ts,void))|G]):-getTypesArgs(Ar,Ts),getA(Ar,T2),append(T2,G,ArG),block(B,ArG,void).
def(procRec(Nom,Ar,B),G,[(Nom,typefun(Ts,void))|G]):-getTypesArgs(Ar,Ts),getA(Ar,T2),append(T2,G,ArG),block(B,[(Nom,typefun(Ts,void))|ArG],void).

%stat(predicat,contexte,void)

stat(echo(E),G,void):-expr(E,G,int).
stat(set(Nom,E),G,void):-assoc(Nom,G,T),expr(E,G,T).
stat(ifStat(E,B1,B2),G,void):-expr(E,G,bool),block(B1,G,void),block(B2,G,void).
stat(while(E,B),G,void):-expr(E,G,bool),block(B,G,void).
stat(call(Nom,Es),G,void):-typesliste2(G,Es,Ts),assoc(Nom,G,typefun(Ts,void)).

%cmds(predicat,contexte,void)

cmds([Def|Cmds],G,void):-def(Def,G,G1),cmds(Cmds,G1,void).
cmds([Stat|Cmds],G,void):-stat(Stat,G,void),cmds(Cmds,G,void).
cmds([],_,void).

%block(predicat,contexte,void)
block(block(CS),G,void):-cmds(CS,G,void).

%prog(predicat,void)

typeprog(prog(B)):-block(B,[],void).


main_stdin :-
    read(user_input, T),
    typeCheck(T, R),
    print(R),
    nl,
    exitCode(R).

typeCheck(P, ok) :- typeprog(P).
typeCheck(_, ko).

exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).