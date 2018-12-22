:- module(parser,[parse/2,parseFile/2]).

parse(File,Bs):- setup_call_cleanup(open(File,read,F),(
    parseFuns(F,As,[]),!,poses(F,As,Bs)
  ),close(F)).

parseFuns(F,O,O) :- at_end_of_stream(F).
parseFuns(F,[A|I],O) :- parseFun(F,A),parseFuns(F,I,O).
parseFun(F,A:SP) :- read_clause(F,A,[subterm_positions(SP)]),parseSpaces(F).
parseSpaces(F) :- peek_char(F,C),space(C) -> get_char(F,_),parseSpaces(F); true.
space(' '). space('\r'). space('\n'). space('\t').

% append position datas to terms
poses(F,As,As_) :- maplist(pos(F),As,As_).
pos(F,(A:-B):term_position(_,_,_,_,[AP,BP]),(A_:-B_)) :- pos(F,A:AP,A_),pos(F,B:BP,B_).
pos(F,(A,B):term_position(_,_,_,_,[AP,BP]),(A_,B_)) :- pos(F,A:AP,A_),pos(F,B:BP,B_).
pos(F,A:B,L:P:A) :- (B=term_position(P1,_,_,_,_);B=P1-_),
                    seek(F,P1),line_count(F,L),line_position(F,P).
seek(F,N) :- seek(F,0,current,P),
  ( N=P->true
  ; N>P->get_char(F,_),seek(F,N)
  ; seek(F,0,bof,0),
    set_stream_position(F,'$stream_position'(0,1,0,0)),
    seek(F,N)).

% print

fun(A:-B)   :- format('~w ~`.t~50|\n',[A]),term(B).
term((A,B)) :- term(A),term(B).
term(A)     :- writeln(A).

parseFile(File,Cs) :- parse(File,Bs), maplist(pfun,Bs,Cs1),append(Cs1,Cs).
pfun(_:-B,R) :- pterm(B,R,[]).
pterm((A,B)) --> pterm(A),pterm(B).
pterm(A)     --> [A].

%:- parse("pasm.txt",Bs),maplist(fun,Bs).
%:- halt.
