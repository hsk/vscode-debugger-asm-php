#!/usr/bin/php
<?php
server();

function readData($fp) {
  $len = 0;
  $data = "";
  while(!feof($fp)) {
    $stdin = trim(fgets($fp));
    if ($stdin === '') break;
    if (preg_match('/^Content-Length: (\\d+)/',$stdin, $m)>0) $len = $m[1];
  }
  if($len <= 0) return false;
  return fread($fp,$len);
}

function sendMessage($fp,$data) {
  fprintf($fp, "Content-Length: %d\r\n\r\n%s", strlen($data),$data);
}
function sendJSON($arr) {
  $arr["seq"]=State::seq();
  sendMessage(STDOUT,json_encode($arr));
}
function sendEvent($event,$body=null) {
  $arr = array("type"=>"event","event"=>$event);
  if($body) $arr["body"]=$body;
  sendJSON($arr);
}
function sendResponse($arr) {
  sendJSON($arr);
}

function sendOutput($msg) {
  sendEvent("output",array("category"=>"console","output"=>$msg));
}

function server() {
  $fp=fopen("/tmp/server.log","a");
  fprintf($fp,"listen\n");
  while(true) {
      $data = readData(STDIN);
      if($data===false) break;
      $data = json_decode(trim($data),true);
      $command = $data["command"];
      if($command=="disconnect") break;
      switch($data["command"]) {
        case "initialize":
        case "launch":
        case "setBreakpoints":
        case "setExceptionBreakpoints":
        case "threads":
          $response=array(
            "type"=>"response",
            "request_seq"=>$data["seq"],
            "command"=>$data["command"],
            "success"=>true,
          );
          if($data["arguments"]) $response["body"]=$data["arguments"];
          $command($response,$data);
          continue 2;
      }
      sendOutput('unknown command '.json_encode($data)."\n");
  }
  fprintf($fp, "end\n");
  fclose($fp);
}
function initialize($response,$param) {
  /*
  Content-Length: 277

  {"command":"initialize","arguments":{"clientID":"vscode","adapterID":"asm-php","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"ja"},"type":"request","seq":1}
  */
  sendEvent("initialized");
  sendOutput("test start initialize\n");
  $response["body"] = $response["body"] || array();
  $response["body"]["supportsConfigurationDoneRequest"]=true;
  $response["body"]["supportsEvaluateForHovers"]=true;
  sendResponse($response);
  sendOutput("test start send response\n"); 
}

function launch($response,$param) {
  sendOutput("launch o\n");
  sendOutput("1\n");
  sendResponse($response);
  sendEvent("stopped",array("reason"=>"step","threadId"=>1));
  sendEvent("terminated");
}
function setBreakpoints($response,$param) {
  //sendMessage(STDOUT,'b');
  /*
  {"command":"setBreakpoints","arguments":{"source":{"name":"asm.txt","path":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt"},"lines":[2],"breakpoints":[{"line":2}],"sourceModified":false},"type":"request","seq":2}
  */
  $response["body"]=array("breakpoints"=>array(array("verified"=>true,"line"=>2,"id"=>1000 ) ) );
  sendResponse($response);
  sendOutput("setBreakpoints ok\n");
}

function setExceptionBreakpoints($response,$param) {
  /*
  Content-Length: 99

  {"command":"setExceptionBreakpoints","arguments":{"filters":["uncaught"]},"type":"request","seq":4}
  */
  sendResponse($response);
}

function threads($response,$param) {
  /*
  {"command":"threads","type":"request","seq":5}
  '{"seq":2,"type":"response","request_seq":1,"command":"initialize","success":true,"body":{"supportsConfigurationDoneRequest":true,"supportsEvaluateForHovers":true}}'
  */
  $response["body"] = array("threads"=>array(array("id"=>1,"name"=>"thread 1")));
  sendResponse($response);
}

class State {
  static $seq=0;
  static function seq() {
    return State::$seq++;
  }
}
