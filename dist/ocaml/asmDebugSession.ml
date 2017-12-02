open DebugSession

let thread_id = 1
let _breakpointId = ref 999
let _sourceFile = ref ""
let _breakPoints: (string * Yojson.Basic.json list) list ref = ref []
let _variableHandles = ref []

let () =
  (*setDebuggerLinesStartAt1(false);*)
  (*setDebuggerColumnsStartAt1(false);*)
  VM.logger := (fun a -> log1(a); log(a))

open Yojson.Basic.Util
let unwrapAssoc : Yojson.Basic.json -> (string * Yojson.Basic.json)list = function (`Assoc a) -> a
let initializeRequest((`Assoc response:Yojson.Basic.json), args) =
  sendEvent0("initialized");
  let body = if response |> List.mem_assoc "body" then response |> List.assoc "body" |> unwrapAssoc else [] in
  let body = (body @ ["supportsConfigurationDoneRequest",`Bool true; "supportsEvaluateForHovers",`Bool true]) in
  sendResponse(`Assoc (Ast.update "body" (`Assoc body) response))

let setBreakpointsRequest((`Assoc response:Yojson.Basic.json), args) =
  let path = args |> member "source" |> member "path" |> to_string in
  let lines = try args|> member "lines" |> to_list |> List.map to_int with _ -> [] in
  let codes = (VM.parseFile path).codes in
  let breakpoints = lines |> List.map (fun line ->
    let rec loop l v j =
      if j >= Array.length codes - 1 then (l,v) else
      if codes.(j).line >= l then (codes.(j).line,true)
      else loop l v (j+1)
    in
    let l,v = loop (convertClientLineToDebugger line) false 0 in
    let l,v = if v || Array.length codes <= 0 then l,v
              else codes.(Array.length codes - 1).line,true in
    incr _breakpointId;
    `Assoc ["verified",`Bool v; "line",`Int (convertDebuggerLineToClient l); "id",`Int !_breakpointId]
  ) in
  sendResponse(`Assoc (response @ ["body",`Assoc["breakpoints",`List breakpoints]]));
  _breakPoints := Ast.update path breakpoints !_breakPoints
  
let threadsRequest((`Assoc response:Yojson.Basic.json),args) =
  sendResponse(`Assoc (response @ ["body",`Assoc["threads", `List[`Assoc["id",`Int thread_id; "name",`String "thread 1"]]]]))

let sendTerminated(response) =
  sendResponse(response);
  sendEvent0("terminated")

let sendStoped(response,reason) =
  sendResponse(response);
  sendEvent("stopped",`Assoc["reason",`String reason;"threadId",`Int thread_id])

(* ブレークポイントや例外が発生したらブレークする *)
let hitBreakPoint() =
  try (* ブレークポイントがあれば止める *)
    !_breakPoints |> List.assoc !_sourceFile |> List.exists (fun bp ->
      bp |> member "line" |> to_int = convertDebuggerLineToClient(VM.getLine())
    )
  with _ -> false
  
(* ▶ ボタンを押した時に呼ばれる *)
let continueRequest(response, args) =
  let rec loop () =
    if VM.step() then sendTerminated(response)
    else if hitBreakPoint() then sendStoped(response,"breakpoint")
    else loop ()
  in loop ()

(* ステップオーバー *)
let nextRequest(response, args) =
  let len = List.length !VM.frames in
  let rec loop () =
    if VM.step() then sendTerminated(response)
    else if hitBreakPoint() then sendStoped(response,"breakpoint")
    else if len >= List.length !VM.frames then sendStoped(response,"step")
    else loop ()
  in loop ()

let stepInRequest(response, args) =
  if VM.step() then sendTerminated(response) else sendStoped(response,"step")

let stepOutRequest(response, args) =
  let len = List.length !VM.frames in
  let rec loop () =
    if VM.step() then sendTerminated(response)
    else if hitBreakPoint() then sendStoped(response,"breakpoint")
    else if List.length !VM.frames < len then sendStoped(response,"step")
    else loop ()
  in loop ()  

let launchRequest(response, args) =
  _sourceFile := args |> member "program" |> to_string;
  VM.loadFile(!_sourceFile);
  if args|> unwrapAssoc |> List.mem_assoc "stopOnEntry" then sendStoped(response,"entry")
  else if hitBreakPoint() then sendStoped(response,"breakpoint")
  else continueRequest(response, `Assoc["threadId",`Int thread_id])

let rec tail s f = match (s,f) with
  | n,f when n <= 0 -> f
  | n,x::xs -> tail (n-1) xs
let rec head e f = match (e,f) with
  | n,_ when n <= 0 -> []
  | n,x::xs -> x::head (n-1) xs
let slice s e l = head (e-s) (tail s l)

let convertDebuggerPathToClient(p) = p

let stackTraceRequest((`Assoc response:Yojson.Basic.json), args) =
  let (_,frames,code) = !VM.frames |> List.fold_left(fun (id,frames,code) frame ->
    let r = `Assoc[
      "id",`Int id;"name",`String frame.VM.nm;
      "line",`Int (convertDebuggerLineToClient (code.VM.line)); "column",`Int 0;
      "source",`Assoc["name",`String (Filename.basename !_sourceFile);
                      "path",`String (convertDebuggerPathToClient !_sourceFile);
                      "sourceReference",`Int 0]] in
    (id-1, r::frames,VM.getCode(frame.pos))
  ) (List.length !VM.frames,[],VM.getCode0()) in
  let start  = try args |> member "startFrame" |> to_int with _ -> 0 in
  let levels = try args |> member "levels" |> to_int with _ -> List.length frames in
  sendResponse(`Assoc (response @ ["body", `Assoc[
    "stackFrames", `List (slice start (min (List.length frames) (start+levels)) (List.rev frames));
    "totalFrames", `Int (List.length frames)
  ]]))

let handeler_id = ref 999
let createHandler(v) =
  incr handeler_id;
  _variableHandles := (!handeler_id,v) :: !_variableHandles;
  !handeler_id

let scopesRequest((`Assoc response:Yojson.Basic.json), args) =
  let frameReference = args |> member "frameId" |> to_int in
  let scopes = [`Assoc["name",`String "Local"; "variablesReference",`Int (createHandler(frameReference)); "expensive",`Bool false]] in
  sendResponse(`Assoc (response @ ["body",`Assoc["scopes",`List scopes]]))

let variablesRequest((`Assoc response:Yojson.Basic.json), args) =
  let variables = if !_variableHandles |> List.mem_assoc (args|>member "variablesReference"|>to_int) then
    !VM._vars |> List.map(fun (i,v) ->
      `Assoc["name", `String i; "type", `String "integer"; "value", `String (string_of_int v); "variablesReference", `Int 0]
    )
  else [] in
  sendResponse(`Assoc (response @ ["body",`Assoc["variables",`List variables]]))

let setVariableRequest((`Assoc response:Yojson.Basic.json), (`Assoc args:Yojson.Basic.json)) =
  log "setVariable";
  log1 "setVariable****************";
  let name = (`Assoc args) |> member "name" |> to_string in
  let value = try (`Assoc args) |> member "value"|> to_string |> int_of_string with _ -> 0 in
  VM.setValue(name, value);
  let args =Ast.update name (`String (string_of_int value)) args in
  sendResponse(`Assoc (response @ ["body", `Assoc args]))

let evaluateRequest((`Assoc response:Yojson.Basic.json), args) =
  try
  sendResponse(`Assoc (response @ ["body", `Assoc[
    "result", `String (VM.getValue_(VM.parseImm(args |> member "expression" |> to_string)) |> string_of_int);
    "variablesReference" , `Int 0
  ]]))
  with _ -> sendResponse(`Assoc (response))