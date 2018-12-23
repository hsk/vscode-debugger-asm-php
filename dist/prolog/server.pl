#!/usr/bin/env swipl
:- use_module(library(http/json)),use_module(parser),use_module(vm).

% DebugSession
:- nb_setval(lineAt1,true),nb_setval(columnAt1,true),nb_setval(seq,1).
:- nb_setval(last_request_seq,0),dynamic(breakPoints/2).
setDebuggerLinesStartAt1(Bool):- nb_setval(lineAt1,Bool).
setDebuggerColumnsStartAt1(Bool):- nb_setval(columnAt1,Bool).
convertClientLineToDebugger(I,I):- nb_getval(lineAt1,true),!.
convertClientLineToDebugger(I,I1):- nb_getval(lineAt1,false),!,I1 is I-1.
convertDebuggerLineToClient(I,I):- nb_getval(lineAt1,true),!.
convertDebuggerLineToClient(I,I1):- nb_getval(lineAt1,false),!,I1 is I+1.
convertDebuggerPathToClient(P,P).
inc(N,V):-nb_getval(N,V),V1 is V+1,nb_setval(N,V1),!.
sendJSON(Arr):- inc(seq,Seq),atom_json_dict(Str,Arr.put(seq,Seq),[width(0)]),!,
                log1('<~w\r\n',[Str]),atom_length(Str,L),
                format('Content-Length: ~d\r\n\r\n~s',[L,Str]),flush_output.
event(Event):- sendJSON(_{type:event,event:Event}).
event(Event,Body):- sendJSON(_{type:event,event:Event,body:Body}).
response(Arr):- nb_getval(last_request_seq,S),Arr.get(request_seq)=S,!.
response(Arr):- inc(last_request_seq,_),!,sendJSON(Arr).
terminated(Res):- response(Res),event(terminated).
stopped(Res,Reason):- response(Res),thread_id(Id),
                      event(stopped,_{reason:Reason,threadId:Id}).
log(M):- sformat(N,'~w\n',[M]),event(output,_{category:console,output:N}).
log(M,Ls):- sformat(A,M,Ls),log(A).
log1(L,Ls):- format(user_error,L,Ls),!.
log1(L):- writeln(user_error,L),!.
log2(Str):- open('/tmp/recive.log',append,H),writeln(H,Str),close(H).
trim(A,R):- normalize_space(atom(R),A).
readData(R):- readData(0,R).
readData(Len,R):-
  prompt1(''),\+ at_end_of_stream,
  read_line_to_codes(user_input,IN1),atom_codes(IN,IN1),trim(IN,IN_),!,
  (IN_='',!,readData2(Len,R)
  ;atom_concat('Content-Length: ',Len2,IN_),!,readData(Len2,R)
  ;readData(Len,R)).
readData2(0,_):- !,false.
readData2(Len,R):- !,atom_number(Len,Len2),prompt1(''),
                   read_string(user_input,Len2,Str),!,atom_string(R,Str),!.
dispatch(Data):- trim(Data,Data_),log2(Data),log1(Data),
  atom_json_dict(Data_,Data2,[value_string_as(atom)]),Com = Data2.command,
  (\+current_predicate(pub:Com/2) -> log('unknown command ~w',[Data_])
  ; Res= _{type:response,request_seq:Data2.get(seq),command:Com,success:true},
    (Res2 = Res.put(body,Data2.get(body));Res2=Res),
    (Args=Data2.get(arguments); Args=null),
    nb_setval(last_request_seq,0),call(pub:Com,Res2,Args),
    log1('call method ok-------------------'),(Com=disconnect,halt;true)).
dispatch:- readData(Data),dispatch(Data),dispatch. dispatch.
% 例外時ブレークポイント
pub:setExceptionBreakpoints(Res,_):- response(Res).
% 設定終了
pub:configurationDone(Res,_):- response(Res).
% 接続終了
pub:disconnect(Res,_):- response(Res),log1(disconnect),log1('exit process').

% AsmDebugSession
thread_id(1).
:- nb_setval(breakpointId,1000),nb_setval(handeler_id,1000),
   setDebuggerLinesStartAt1(true),setDebuggerColumnsStartAt1(true).
% 初期化
pub:initialize(Res,_):- event(initialized),(Body=Res.get(body); Body=_{}),
  response(Res.put(body,
    Body.put([supportsConfigurationDonecommand:true,
              %supportsSetExpression:true,
              supportsSetVariable:true,supportsEvaluateForHovers:true]))).
% ブレークポイントの設定
pub:setBreakpoints(Res,Args):-
  (Lines = Args.get(lines);Lines=[]),!,Path = Args.source.path,
  catch(parseFile(Path,Codes),Error,(log1('error:~w\n',Error),false)),
  maplist(setBreakpoints1(Codes),Lines,Breakpoints),
  retractall(breakPoints(Path,_)),assert(breakPoints(Path,Breakpoints)),
  response(Res.put(body,_{breakpoints:Breakpoints})),!.
pub:setBreakpoints(Res,_):- response(Res.put(body,_{breakpoints:[]})).
setBreakpoints1(Codes,Line,_{verified:V2,line:L4,id:Id}):-
  convertClientLineToDebugger(Line,L1),setBreakpoints2(L1,Codes,L2,V),
  (V=false,last(Codes,L3:_:_) -> V2=true;V2=V,L3=L2),
  convertDebuggerLineToClient(L3,L4),inc(breakpointId,Id),!.
setBreakpoints2(L,[],L,false).
setBreakpoints2(L,[L2:_:_|_],L2,true):- L2 >= L,!.
setBreakpoints2(L,[_|Cs],L2,V):- setBreakpoints2(L,Cs,L2,V).
% スレッドの取得
pub:threads(Res,_):-
  thread_id(Id),response(Res.put(body,_{threads:[_{id:Id,name:'thread 1'}]})).
% デバッガ起動時の最初の実行
pub:launch(Res,Args):- nb_setval(file,Args.program),vm:loadFile(Args.program),
                       ( true=Args.get(stopOnEntry)->stopped(Res,entry)
                       ; ( hitBreakPoint(true) -> stopped(Res,breakpoint)
                         ; thread_id(Id),pub:continue(Res,_{threadId:Id}))).
% ▶ ボタンを押した時に呼ばれる
pub:continue(Res,_):- \+vm:step,!,terminated(Res).
pub:continue(Res,_):- hitBreakPoint(true),!,stopped(Res,breakpoint).
pub:continue(Res,Args):- pub:continue(Res,Args).
hitBreakPoint(R):- nb_getval(file,File),breakPoints(File,Bps1),
                   vm:getLine(Line),include(hitBreakPointFilter(Line),Bps1,Bps),
                   (Bps=[]->R=false;R=true),!.
hitBreakPointFilter(Line,Bp):- convertDebuggerLineToClient(Line,Bp.line).
% スタックトレース
pub:stackTrace(Res,Args):- nb_getval(file,File),file_base_name(File,Name),
  convertDebuggerPathToClient(File,Path),
  Source=_{name:Name,path:Path,sourceReference:0},
  vm:frames(Frames),length(Frames,L),vm:getLine(Line),
  foldl(stackTrace1(Source),Frames,(L,[],Line),(_,Frames2,_)),!,
  (Start  = Args.get(startFrame) ; Start = 0),
  (Levels = Args.get(levels) ; Levels = L),End is min(L,Start+Levels)-1,
  findall(F,(between(Start,End,I),nth0(I,Frames2,F)),Fs),reverse(Fs,RFs),
  response(Res.put(body,_{stackFrames: RFs,totalFrames: L})).
stackTrace1(Source,Frame,(Id,Frames1,Line1),(Id1,[R|Frames1],Line2)):-
  convertDebuggerLineToClient(Line1,Line),
  R=_{id:Id,name: Frame.nm,source:Source,line:Line,column:0},
  vm:code(Frame.pos=Line2:_:_),Id1 is Id-1.
% ステップオーバー
pub:next(Res,Args):- vm:frameLength(L),!,next(Res,Args,L).
next(Res,_,_):- \+vm:step,!,terminated(Res).
next(Res,_,_):- hitBreakPoint(true),!,stopped(Res,breakpoint).
next(Res,_,Len):- vm:frameLength(L),Len >= L,!,stopped(Res,step).
next(Res,Args,Len):- next(Res,Args,Len).
% ステップイン
pub:stepIn(Res,_):- \+vm:step,!,terminated(Res).
pub:stepIn(Res,_):- stopped(Res,step).
% ステップアウト
pub:stepOut(Res,Args):- vm:frameLength(L),stepOut(Res,Args,L).
stepOut(Res,_,_):- \+vm:step,!,terminated(Res).
stepOut(Res,_,_):- hitBreakPoint(true),!,stopped(Res,breakpoint).
stepOut(Res,_,Len):- vm:frameLength(L),L < Len,!,stopped(Res,step).
stepOut(Res,Args,Len):- stepOut(Res,Args,Len).
% 変数のスコープ
pub:scopes(Res,Args):- concat(global_,Args.frameId,F),createHandler(F,Ref),
  response(Res.put(body,_{
    scopes:[_{name:'Local',variablesReference:Ref,expensive:false}]})).
createHandler(V,HId):- inc(handeler_id,HId),assert(variableHandles(HId = V)).
% 変数の値の連想配列取得
pub:variables(Res,Args):- variableHandles(Args.variablesReference=_),
  nb_getval(vars,Vars),dict_pairs(Vars,_,Pairs),
  findall(_{name:I,type:integer,value:A,variablesReference:0,storageItem:A},(
    member(I-V,Pairs),sformat(A,'~w',[V])
  ),Vs),
  response(Res.put(body,_{variables:Vs})).
pub:variables(Res,_):- response(Res.put(body,_{variables:[]})).
% 変数設定
pub:setVariable(Res,Args):- atom_number(Args.value,N),vm:set(Args.name,N),
  sformat(A,'~w',[N]),response(Res.put(body,_{value:A,variablesReference: 0})).
% 変数名などから計算
pub:evaluate(Res,Args):-
  vm:parseImm(Args.expression,E),nb_getval(vars,V),sformat(A,'~w',[V.get(E)]),
  response(Res.put(body,_{result:A,variablesReference:0})).
pub:evaluate(Res,_):- response(Res).

:- log1("**************************************************"),dispatch.
