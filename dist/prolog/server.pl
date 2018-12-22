#!/usr/bin/env swipl
:- use_module(library(http/json)).
:- use_module(parser).
:- use_module(vm).

% DebugSession

:- nb_setval(lineAt1, true).
:- nb_setval(columnAt1, true).
:- nb_setval(seq, 1).
:- nb_setval(last_request_seq, 0).
:- dynamic(breakPoints/2).

setDebuggerLinesStartAt1(Bool) :- nb_setval(lineAt1, Bool).
setDebuggerColumnsStartAt1(Bool) :- nb_setval(columnAt1, Bool).
convertClientLineToDebugger(I, I) :- nb_getval(lineAt1, true), !.
convertClientLineToDebugger(I, I1) :- nb_getval(lineAt1,false),!,I1 is I-1.
convertDebuggerLineToClient(I, I) :- nb_getval(lineAt1, true), !.
convertDebuggerLineToClient(I, I1) :- nb_getval(lineAt1,false),!,I1 is I+1.

get_seq(Seq) :- nb_getval(seq, Seq),Seq_ is Seq+1,nb_setval(seq, Seq_), !.
sendJSON(Arr) :-
  get_seq(Seq),atom_json_dict(Str, Arr.put(seq,Seq), [width(0)]), !,
  log1('<~w\r\n', [Str]),atom_length(Str, L),
  format('Content-Length: ~d\r\n\r\n~s', [L, Str]),flush_output.
%:- sendJSON(json([a=a])).
sendEvent(Event) :- sendJSON(_{type:event, event:Event}).
sendEvent(Event, Body) :- sendJSON(_{type:event, event:Event, body:Body}).

sendResponse(Arr) :- nb_getval(last_request_seq, S),Arr.get(request_seq)=S, !.
sendResponse(Arr) :-
    nb_getval(last_request_seq, S),S_ is S+1,nb_setval(last_request_seq, S_), !,
    sendJSON(Arr).
log(Msg) :-
    atomic_list_concat(Msg,Msg1),
    atom_concat(Msg1, '\n', Msg_),
    sendEvent(output, _{category:console, output:Msg_}).
log1(L, Ls) :- format(user_error, L, Ls), !.
log1(L) :- writeln(user_error, L), !.
log2(Str) :- open('/tmp/recive.log', append, Handle),
             writeln(Handle, Str),close(Handle).

trim(A,R) :- (atom_concat(' ',R1,A);atom_concat(R1,' ',A);
             atom_concat('\r',R1,A);atom_concat(R1,'\r',A);
             atom_concat('\n',R1,A);atom_concat(R1,'\n',A)),!,trim(R1,R).
trim(A,A).

readData(R) :- readData(0,R).
readData(Len,R) :-
    prompt1(''),
    \+ at_end_of_stream,
    read_line_to_codes(user_input,IN1),atom_codes(IN,IN1),
    trim(IN,IN_),!,
    (IN_='',!,readData2(Len,R)
    ;atom_concat('Content-Length: ',Len2,IN_),!,readData(Len2,R)
    ;readData(Len,R)).
readData2(0,_) :- !,false.
readData2(Len,R) :- !,atom_number(Len,Len2), prompt1(''),read_string(user_input,Len2,Str),!,atom_string(R,Str),!.

% Main

dispatch :- log1("**************************************************"),
            dispatch_loop.
dispatch_loop :- readData(Data),dispatch(Data),dispatch_loop.
dispatch_loop.
dispatch(Data) :-
    log2(Data),log1(Data),
    trim(Data,Data_),atom_json_dict(Data_,Data2,[value_string_as(atom)]),writeln(Data2),
    Command = Data2.command,
    writeln(command:Command),
    (\+ current_predicate(command:Command/2) -> log(['unknown command ',Data_])
    ; Res= _{type:response,request_seq:Data2.get(seq),
             command:Data2.command,success:true},
      (Res2 = Res.put(body,Data2.get(body));Res2=Res),
      (Args=Data2.get(arguments); Args=null),
      writeln(args:Args),
      nb_setval(last_request_seq,0),
      call(command:Command,Res2,Args),
      log1('call method ok-------------------'),
      (Command=disconnect, halt;true)
    ).

command:setExceptionBreakpoints(Res, _) :- sendResponse(Res).
command:configurationDone(Res, _) :- sendResponse(Res).
command:disconnect(Res, _) :- sendResponse(Res),log1(disconnect),log1('exit process').

% DebugSession

thread_id(1).

:- nb_setval(breakpointId,1000).
:- setDebuggerLinesStartAt1(true).
:- setDebuggerColumnsStartAt1(true).

command:initialize(Res, _) :-
  sendEvent(initialized),(Body=Res.get(body); Body=_{}),
  sendResponse(Res.put(body,
    Body.put([supportsConfigurationDonecommand:true,
              supportsEvaluateForHovers:true]))).
%:- dispatch('{"command":"initialize","seq":1}').

command:setBreakpoints(Res, Args) :-
  Path = Args.source.path,
  catch(parseFile(Path,Codes),Error,(log1('error:~w\n',Error),false)),
  (Lines = Args.get(lines);Lines=[]),!,
  maplist(setBreakpoints1(Codes),Lines,Breakpoints),
  retractall(breakPoints(Path,_)),assert(breakPoints(Path,Breakpoints)),
  sendResponse(Res.put(body, _{breakpoints:Breakpoints})),!.
command:setBreakpoints(Res, _) :-
  sendResponse(Res.put(body, _{breakpoints:[]})),!.
setBreakpoints1(Codes,Line,_{verified:V2,line:L4,id:Id}) :-
    convertClientLineToDebugger(Line,L1),
    setBreakpoints2(L1,Codes,L2,V),
    (V=false,last(Codes,L3:_:_) -> V2=true;V2=V,L3=L2),
    convertDebuggerLineToClient(L3,L4),
    nb_getval(breakpointId,Id),
    Id1 is Id+1,nb_setval(breakpointId,Id1),!.
setBreakpoints2(L,[],L,false).
setBreakpoints2(L,[L2:_:_|_],L2,true) :- L2 >= L,!.
setBreakpoints2(L,[_|Cs],L2,V) :- setBreakpoints2(L,Cs,L2,V).
last([A],A).
last([_|As],A) :- last(As,A).
%:- dispatch('{"command":"setBreakpoints","seq":1,"arguments":{"source":{"path":"../pasm.txt"},"lines":[1,2,3]}}').

command:threads(Res,_) :- thread_id(Id),
  sendResponse(Res.put(body,_{threads:[_{id:Id, name:'thread 1'}]})).

command:launch(Res, Args) :-
  nb_setval(sourceFile,Args.program),
  vm:loadFile(Args.program),
  (true=Args.get(stopOnEntry)->sendStopped(Res,entry)
  ; ( hitBreakPoint(true) -> sendStopped(Res,breakpoint)
    ; thread_id(Id),command:continue(Res, _{threadId:Id})
    )
  ).

% ▶ ボタンを押した時に呼ばれる
command:continue(Res, _) :- \+vm:step,!,sendTerminated(Res).
command:continue(Res, _) :- hitBreakPoint(true),!,sendStopped(Res,breakpoint).
command:continue(Res, Args) :- command:continue(Res, Args).
sendStopped(Res, Reason) :- sendResponse(Res),thread_id(Id),
                            sendEvent(stopped,_{reason:Reason,threadId:Id}).

% ブレークポイントや例外が発生したらブレークする
hitBreakPoint(R) :-
  % 対象のファイルのブレークポイントを取得する
  nb_getval(sourceFile,File),breakPoints(File,Breakpoints),
  vm:getLine(Line),
  % ブレークポイントがあれば止める
  include(hitBreakPointFilter(Line),Breakpoints,Bps),
  log1(line:Line;bps:Bps;Breakpoints),
  (Bps=[]->R=false;R=true),!.
hitBreakPoint(false).
hitBreakPointFilter(Line,Bp) :-
  convertDebuggerLineToClient(Line,Bp.line).
convertDebuggerPathToClient(P,P).

% スタックトレース
command:stackTrace(Res, Args) :-
  nb_getval(sourceFile,SourceFile),
  convertDebuggerPathToClient(SourceFile,Path),
  file_base_name(SourceFile,Name),
  Source=_{name:Name,path:Path,sourceReference:0},
  vm:frames(Frames),length(Frames,L),vm:getCode0(Code),
  foldl(stackTrace1(Source),Frames,(L,[],Code),(_,Frames2,_)),!,
  length(Frames2,Count),
  (Start  = Args.get(startFrame) ; Start = 0),
  (Levels = Args.get(levels) ; Levels = Count),
  End is min(Count, Start+Levels)-1,
  findall(F,(between(Start,End,I),nth0(I,Frames2,F)),Fs),
  reverse(Fs,RFs),
  sendResponse(Res.put(body,_{stackFrames: RFs,totalFrames: Count})).

stackTrace1(Source,Frame,(Id,Frames1,(Line1:_:_)),(Id1,[R|Frames1],Code2)) :-
  convertDebuggerLineToClient(Line1,Line),
  R = _{id:Id,name: Frame.nm,source:Source,line:Line, column:0},
  vm:getCode(Frame.pos,Code2),Id1 is Id-1.

sendTerminated(Res) :- sendResponse(Res), sendEvent(terminated).

% ステップオーバー
command:next(Res, Args) :-
  vm:frames(Frames),length(Frames,Len),!,next(Res,Args,Len).
next(Res,_,_) :- \+vm:step,!, sendTerminated(Res).
next(Res,_,_) :- hitBreakPoint(true),!, sendStopped(Res,breakpoint).
next(Res,_,Len) :- vm:frames(Frames),length(Frames,L),Len >= L, !,
                   sendStopped(Res,step).
next(Res,Args,Len) :- next(Res,Args,Len).

command:stepIn(Res, _) :- \+vm:step,!, sendTerminated(Res).
command:stepIn(Res, _) :- sendStopped(Res,step).
command:stepOut(Res, Args) :- vm:frames(Fs),length(Fs,Len), stepOut(Res,Args,Len).
stepOut(Res,_,_) :- \+vm:step,!, sendTerminated(Res).
stepOut(Res,_,_) :- hitBreakPoint(true),!, sendStopped(Res,breakpoint).
stepOut(Res,_,Len) :- vm:frames(Fs),length(Fs,L),L < Len,!, sendStopped(Res,step).
stepOut(Res,Args,Len) :- stepOut(Res,Args,Len).

command:scopes(Res, Args) :-
  concat(global_,Args.frameId,FrameRef),createHandler(FrameRef,Ref),
  Scopes =[_{name:'Local', variablesReference:Ref, expensive:false}],
  sendResponse(Res.put(body,_{scopes:Scopes})).

:- nb_setval(handeler_id,1000).
createHandler(V,HId) :-
  nb_getval(handeler_id,HId),HId1 is HId+1,nb_setval(handeler_id,HId1),
  assert(variableHandles(HId = V)).
command:variables(Res, Args) :-
  variableHandles(Args.variablesReference=_),
  nb_getval(vars,Vars),dict_pairs(Vars,_,Pairs),
  findall(_{name:I,type:integer,value:A,variablesReference:0},(
    member(I-V,Pairs),format(atom(A),'~w',[V])
  ),Vs),
  sendResponse(Res.put(body,_{variables:Vs})).
command:variables(Res,_) :- sendResponse(Res.put(body,_{variables:[]})).
command:setVariable(Res, Args) :-
  atom_number(Args.value,N),vm:setValue(Args.name, N),
  format(atom(A),'~w',[N]),
  sendResponse(Res.put(body,_{value:A,variablesReference: 0})).
command:evaluate(Res, Args) :-
  E1=Args.expression,
  vm:parseImm(E1,E),nb_getval(vars,Vars),
  V=Vars.get(E),format(atom(A),'~w',[V]),
  sendResponse(Res.put(body,_{result: A,variablesReference : 0})).
command:evaluate(Res,_) :- sendResponse(Res).

:- dispatch.
