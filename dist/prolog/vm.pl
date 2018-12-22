:- module(vm,[step/1]).
parseImm(Src,Term) :- read_term_from_atom(Src,Term,[]).
frame(Label,P) :- nb_getval(vars,Vars),nb_getval(currentPos,Pos),
                  asserta(frame(_{p:P,vars:Vars,pos:Pos,nm:Label})).
frames(Frames) :- findall(Frame,frame(Frame),Frames).
frameLength(Len) :- frames(Fs),length(Fs,Len).
loadFile(Filename) :-
  retractall(label(_)),retractall(code(_)),retractall(frame(_)),
  parse(Filename,Funs),loadFun(0,Funs),label(main=Pos),
  nb_setval(currentPos, Pos),nb_setval(vars,_{}),frame(main,a).
loadFun(_,[]).
loadFun(I,[_:_:A:-T|Fs]) :- assert(label(A=I)),loadTerm(T,I,I2),loadFun(I2,Fs).
loadTerm((A,B)) --> loadTerm(A),loadTerm(B).
loadTerm(T,I,I1) :- I1 is I+1, assert(code(I=T)).

get_(I,I) :- integer(I),!.
get_(A,V) :- atom(A),nb_getval(vars,Vars),V=Vars.get(A).
get(I,V) :- get_(I,V),!.
get(A,0) :- atom(A).

getLine(L) :- nb_getval(currentPos,P),code(P=L:_:_).
getCode(P,C) :- code(P=C).
getCode0(C) :- nb_getval(currentPos,P),code(P=C).
set(Reg, V) :- nb_getval(vars,Vars),nb_setval(vars,Vars.put(Reg,V)).
next :- nb_getval(currentPos,P),P1 is P+1,nb_setval(currentPos,P1),code(P1=_).
step :- nb_getval(currentPos,P), code(P=(_:_:Code)),step(Code).
step(add(A,B,C)) :- get(A,I1),get(B,I2),I is I1+I2,set(C,I),next.
step(sub(A,B,C)) :- get(A,I1),get(B,I2),I is I1-I2,set(C,I),next.
step(mul(A,B,C)) :- get(A,I1),get(B,I2),I is I1*I2,set(C,I),next.
step(div(A,B,C)) :- get(A,I1),get(B,I2),I is I1 div I2,set(C,I),next.
step(print(A)) :- get(A,V),log([V]),next.
step(ret(A)) :-
  frames(Frames),length(Frames,Len),Len>1 ->
    retract(frame(F)),get(A,V),NF=F.put(vars,F.vars.put(F.p, V)),
    nb_setval(vars,NF.vars),nb_setval(currentPos,F.pos),next
  ; findall(_,code(_=_),L),length(L,Len2),nb_setval(currentPos,Len2),!,false.
step(call(Label,Params,P)) :-
  label(Label=Pos),code(Pos=_:_:C),enter(Enter)=C,frame(Label,P),
  maplist(get,Params,Ps2),maplist([A,B,A=B]>>!,Enter,Ps2,Vars1),
  Vars = _{}.put(Vars1),nb_setval(vars, Vars),nb_setval(currentPos, Pos),next.
step(call(Label,_,_)) :- error("not found label ~w", [Label]).
