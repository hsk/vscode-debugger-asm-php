let lineAt1 = ref true
let columnAt1 = ref true
let setDebuggerLinesStartAt1(bool) = lineAt1 := bool
let setDebuggerColumnsStartAt1(bool) = columnAt1 := bool
let convertClientLineToDebugger(i) = if !lineAt1 then i else i-1
let convertDebuggerLineToClient(i) = if !columnAt1 then i else i+1

let log1(str) = Printf.fprintf stderr "%s\n%!" str
let log2(str) =
  let fp = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 "/tmp/recive.log" in
  Printf.fprintf fp "%s\n%!", str;
  close_out fp

let json_encode(arr) = Yojson.Basic.to_string(arr)
let json_decode(arr) = Yojson.Basic.from_string(arr)
let seq = ref 1
let sendJSON = function
  |`Assoc(data) ->
    let data = ("seq",`Int !seq) :: data in
    incr seq;
    let str = json_encode(`Assoc(data)) in
    log1("<" ^ str);
    Printf.printf "Content-Length: %d\r\n\r\n%s%!"  (String.length str) str
  | _ -> assert false


let sendEvent(event,body) = sendJSON(`Assoc["type",`String"event";"event",`String event;"body",body])
let sendEvent0(event) = sendJSON(`Assoc["type",`String"event";"event",`String event])
let log(msg) = sendEvent("output",`Assoc["category",`String"console";"output",`String(msg^"\n")])

open Yojson.Basic.Util

let last_request_seq = ref 0
let sendResponse(arr:Yojson.Basic.json) =
  if !last_request_seq = (try arr |> member "request_seq" |> to_int with _ -> 0) then () else
  last_request_seq := (try arr |> member "request_seq" |> to_int with _ -> 0);
  sendJSON(arr)

let startWith str1 str2 =
  let len1 = String.length str1 in
  let len2 = String.length str2 in
  if len1 < len2 then false else (
  let str1 = String.sub str1 0 len2 in
  str1 = str2)

let readData(fp) =
  let rec loop len =
    try 
    let stdin = String.trim(input_line fp) in
    if stdin = "" then len else
    let cl = "Content-Length: " in
    let cllen = String.length cl in
    if not (startWith stdin cl) then loop len else
    loop (int_of_string(String.sub stdin cllen ((String.length stdin) - cllen)))
    with End_of_file -> len
  in let len = loop 0 in
  if len <= 0 then None else Some (really_input_string fp len)

let setExceptionBreakpointsRequest(response, argv) = sendResponse(response)
let configurationDoneRequest(response, argv) = sendResponse(response)

let disconnectRequest(response, argv) =
  sendResponse(response);
  log("disconnect");
  log("exit process")
