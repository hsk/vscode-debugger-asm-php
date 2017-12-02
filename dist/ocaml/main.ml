open DebugSession
open AsmDebugSession
open Yojson.Basic.Util
let dispatch () =
  log1("**************************************************");
  let rec loop () =
    match readData(stdin) with
    | None -> ()
    | Some data ->
      log2(data);
      log1(data);
      let data = try json_decode(String.trim(data)) with _ -> (log1 "command error\n"; `Assoc []) in
      let response=[
        "type",`String "response";
        "request_seq",`Int (try data|>member "seq"|>to_int with _ -> 0);
        "command",`String (try data|> member "command" |> to_string with _ -> "unknown");
        "success",`Bool true;
      ] in
      let response = if data|>unwrapAssoc|>List.mem_assoc "body"
        then response @ ["body",data|>member "body"] else response in
      let args = try data |> member "arguments" with _ -> `Assoc [] in
      last_request_seq:=0;
      let command = try data |> member "command" |> to_string with _ -> "unknown" in
      let fn = match command with
        | "initialize" -> initializeRequest
        | "setBreakpoints" -> setBreakpointsRequest
        | "setExceptionBreakpoints" -> setExceptionBreakpointsRequest
        | "configurationDone" -> configurationDoneRequest
        | "disconnect" -> disconnectRequest
        | "threads" -> threadsRequest
        | "launch" -> launchRequest
        | "stackTrace" -> stackTraceRequest
        | "next" -> nextRequest
        | "stepIn" -> stepInRequest
        | "stepOut" -> stepOutRequest
        | "continue" -> continueRequest
        | "scopes" -> scopesRequest
        | "variables" -> variablesRequest
        | "setVariable" -> setVariableRequest
        | "evaluate" -> setVariableRequest
        | cmd -> (fun (a,b) -> log("unknown command "^json_encode(data)))
      in
      fn(`Assoc response,args);
      log1("call method ok------------------- ["^command^"]");
      if command = "disconnect" then () else loop ()
  in loop()

let () = dispatch ()
