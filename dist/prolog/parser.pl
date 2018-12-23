:- module(parser,[parse/2,parseFile/2]).
parse(File,Bs):- setup_call_cleanup(open(File,read,F),(
  parseFuns(F,As,[]),!,maplist(pos(F),As,Bs)),close(F)).
parseFuns(F,O,O):- at_end_of_stream(F).
parseFuns(F,[A|I],O):- parseFun(F,A),parseFuns(F,I,O).
parseFun(F,A:SP):- read_clause(F,A,[subterm_positions(SP)]),parseSpaces(F).
parseSpaces(F):- peek_char(F,C),space(C) -> get_char(F,_),parseSpaces(F); true.
space(' '). space('\r'). space('\n'). space('\t').
pos(F,(A:-B):term_position(_,_,_,_,[I,J]),A_:-B_):- pos(F,A:I,A_),pos(F,B:J,B_).
pos(F,(A,B):term_position(_,_,_,_,[I,J]),(A_,B_)):- pos(F,A:I,A_),pos(F,B:J,B_).
pos(F,A:B,L:P1:A):- (B=term_position(P,_,_,_,_);B=P-_),seek(F,P,L:P1).
seek(F,N,L:P1):- seek(F,0,current,P),
  ( N=P->line_count(F,L),line_position(F,P1)
  ; N>P->get_char(F,_),seek(F,N,L:P1)
  ; seek(F,0,bof,0),set_stream_position(F,'$stream_position'(0,1,0,0)),
    seek(F,N,L:P1)).
parseFile(File,Cs):- parse(File,Bs),maplist(pfun,Bs,Cs1),append(Cs1,Cs).
pfun(_:-B,R):- pterm(B,R,[]).
pterm((A,B)) --> pterm(A),pterm(B).
pterm(A)     --> [A].
