:- module(vm,[step/1]).
parseImm(Src,Term):- read_term_from_atom(Src,Term,[]).
frame(Label,Ps,P):- nb_getval(vars,Vars),nb_setval(vars,_{}),nb_getval(pc,Pos),
                    asserta(frame(_{p:P,ps:Ps,vars:Vars,pos:Pos,nm:Label})).
frames(Frames):- findall(Frame,frame(Frame),Frames).
frameLength(Len):- frames(Fs),length(Fs,Len).
loadFile(File):- retractall(label(_)),retractall(code(_)),retractall(frame(_)),
                  parse(File,Funs),foldl(loadFun,Funs,0,_),label(main=Pos),
                  nb_setval(pc,Pos),nb_setval(vars,_{}),frame(main,[],a).
loadFun(_:_:A:-T,I,I1):- assert(label(A=I)),loadTerm(T,I,I1).
loadTerm((A,B)) --> loadTerm(A),loadTerm(B).
loadTerm(T,I,I1):- assert(code(I=T)),I1 is I+1.
get_(I,I):- integer(I),!.
get_(A,V):- atom(A),nb_getval(vars,Vars),V=Vars.get(A).
get(I,V):- get_(I,V),!;atom(I),V=0.
getLine(L):- nb_getval(pc,P),code(P=L:_:_).
set(Reg,V):- nb_getval(vars,Vars),nb_setval(vars,Vars.put(Reg,V)).
next:- nb_getval(pc,P),P1 is P+1,nb_setval(pc,P1),code(P1=_).
step:- nb_getval(pc,P),code(P=_:_:Code),step(Code).
step(add(A,B,C)):- get(A,I1),get(B,I2),I is I1+I2,set(C,I),next.
step(sub(A,B,C)):- get(A,I1),get(B,I2),I is I1-I2,set(C,I),next.
step(mul(A,B,C)):- get(A,I1),get(B,I2),I is I1*I2,set(C,I),next.
step(div(A,B,C)):- get(A,I1),get(B,I2),I is I1 div I2,set(C,I),next.
step(print(A)):- get(A,V),log(V),next.
step(ret(A)):- frames(Frames),length(Frames,Len),Len>1,!,retract(frame(F)),
  get(A,V),nb_setval(vars,F.vars.put(F.p,V)),nb_setval(pc,F.pos),!,next.
step(ret(_)):- findall(_,code(_=_),L),length(L,Len2),nb_setval(pc,Len2),!,false.
step(call(Label,Ps,P)):- frame(Label,Ps,P),label(Label=Pc),nb_setval(pc,Pc).
step(call(Label,_,_)):- error("not found label ~w",[Label]).
step(enter(Enter)):- frame(F),maplist(enter(F),Enter,F.ps),next.
enter(F,A,B):-set(A,F.vars.get(B)).
