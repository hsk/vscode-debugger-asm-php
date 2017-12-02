#!/usr/bin/env swipl
:- expects_dialect(sicstus).
log1(L,Ls) :- format(user_error,L,Ls),!.
log1(L) :- writeln(user_error,L),!.
%:- log1('aaa~w\n',[bbb]).
:- bb_put(lineAt1,true).
:- bb_put(columnAt1,true).
setDebuggerLinesStartAt1(Bool) :- bb_put(lineAt1,Bool).
setDebuggerColumnsStartAt1(Bool) :- bb_put(columnAt1, Bool).
convertClientLineToDebugger(I,I) :- bb_get(lineAt1,true),!.
convertClientLineToDebugger(I,I1) :- !, I1 is I - 1.
convertDebuggerLineToClient(I,I) :- bb_get(lineAt1,true),!.
convertDebuggerLineToClient(I,I1) :- !, I1 is I + 1.
:- use_module(library(http/json)).
%:- atom_json_term('{"a":"abc","d":"efg"}',J,[]),writeln(J).
:- bb_put(seq,1).
get_seq(Seq) :- bb_get(seq,Seq),Seq_ is Seq + 1, bb_put(seq,Seq_),!.
sendJSON(json(Arr)) :-
    get_seq(Seq),
    atom_json_term(Str,json([seq=Seq|Arr]),[]),!,
    log1('<~w\r\n',[Str]),
    atom_length(Str,L),
    format('Content-Length: ~d\r\n\r\n~s', [L,Str]).
%:- sendJSON(json([a=a])).
sendEvent(Event) :- sendJSON(json([type=event,event=Event])).
sendEvent(Event,Body) :- sendJSON(json([type=event,event=Event,body=Body])).
:- bb_put(last_request_seq,0).
sendResponse(json(Arr)) :- bb_get(last_request_seq,S),member(request_seq=S,Arr),!.
sendResponse(json(Arr)) :- bb_get(last_request_seq,S),S_ is S+1,bb_put(last_request_seq,S_),!,sendJSON(json(Arr)).
log(Msg) :- atom_concat(Msg,'\n',Msg_),sendEvent(output,json([category=console,output=Msg_])).
log2(Str) :- open('/tmp/recive.log', append, Handle), writeln(Handle, Str),close(Handle).
/*
%:- log2(aaa),log2(aaa).
readData(R) :-
    Len = 0,
    Data = '',
    while(!feof($fp)) {
      $stdin = trim(fgets($fp));
      if ($stdin === '') break;
      if (preg_match('/^Content-Length: (\\d+)/',$stdin, $m)>0) Len = $m[1];
    }
    if(Len <= 0) return false;
    return fread($fp,Len);
  }

  function dispatch() {
    $this->log1("**************************************************");
    while(true) {
      Data = $this->readData(STDIN);
      if(Data===false) break;
      $this->log2(Data);
      $this->log1(Data);
      Data = $this->json_decode(trim(Data));
      $command = Data["command"];
      switch(Data["command"]) {
      case "initialize": case "setBreakpoints": case "setExceptionBreakpoints":
      case "configurationDone": case "disconnect": case "threads": case "launch":
      case "stackTrace": case "next": case "stepIn": case "stepOut": case "continue":
      case "scopes": case "variables": case "setVariable": case "evaluate":
        Response=array(
          "type"=>"response",
          "request_seq"=>Data["seq"],
          "command"=>Data["command"],
          "success"=>true,
        );
        $command.="Request";
        if(isset(Data["body"])) Response["body"]=Data["body"];
        if(!isset(Data["arguments"])) Data["arguments"]=null;
        $this->last_request_seq=0;
        call_user_func(array($this,$command),Response,Data["arguments"]);
        $this->log1("call method ok-------------------");
        if($command=="disconnectRequest") return;
        break;
      default:
        $this->log('unknown command '.$this->json_encode(Data));
      }
    }
  }

setExceptionBreakpointsRequest(Response, Argv) :- sendResponse(Response).
configurationDoneRequest(Response, Argv) :- sendResponse(Response).
disconnectRequest(Response, Argv) :- sendResponse(Response), log("disconnect"), log("exit process").

class VM {
  private $_currentPos = 0;
  private $_codes = array();
  public $_vars = array();
  private $_labels = array();
  public $frames = array();
  public $logger;
  public function log($str) {
    $loger = $this->logger;
    $loger($str);
  }
  public function __construct() {
    $this->logger = function($str){fprintf(STDERR,"%s\n",$str);};
  }
  // ファイルをパースして構文木データを返す
  static function parseFile($filename) {
    $fp = fopen($filename,"r");
    if($fp===false) {
      $this->log("not open file:".$filename);
      exit(0);
      return false;
    }
    $line = 0; $pos = 0;
    $codes = array(); $labels = array();
    while(!feof($fp)){
      $t = trim(fgets($fp));
      $a = array("data"=>preg_split("/\s+/",$t),"line"=> $line++);
      if(count($a["data"]) == 0) continue;
      if (preg_match('/^([^:]+):$/', $a["data"][0], $m) > 0) {
        $labels[$m[1]]=$pos;
        array_shift($a["data"]);
        if(count($a["data"])==0) continue;
      }
      $pos++;
      switch($a["data"][0]){
      case "add":
      case "sub":
      case "mul":
      case "div":
        if (count($a["data"]) == 4) {$codes[]=$a; continue 2;} break;
      case "ret":
      case "print":
        if (count($a["data"]) == 2) {$codes[]=$a; continue 2;} break;
      case "enter":
        if (count($a["data"]) >= 1) {$codes[]=$a; continue 2;} break;
      case "call":
        if (count($a["data"]) >= 2) {$codes[]=$a; continue 2;} break;
      default:
        break;
      }
      $pos--;
    }
    fclose($fp);
    
    $rc = array("codes"=>$codes,"labels"=>$labels);
    return $rc;
  }
  
  function loadFile($filename) {
    $a = VM::parseFile($filename);
    $this->_codes = $a["codes"];
    $this->_labels = $a["labels"];
    $this->_currentPos = $a["labels"]["main"];
  }
  function getValue(Argv) {
    if(is_numeric(Argv)) return (int)Argv;
    if(isset($this->_vars[Argv])) return $this->_vars[Argv];
    return 0;
  }
  function getLine() {
    return $this->_codes[$this->_currentPos]["line"];
  }
  function getCode($pos=null) {
    if($pos===null) $pos=$this->_currentPos;
    return $this->_codes[$pos];
  }
  function setValue($reg, $v) {
    $this->_vars[$reg]=$v;
  }

  function step() {
    $code = $this->_codes[$this->_currentPos];
    Data = $code["data"];
    switch(Data[0]) {
    case "add":
      $a=$this->getValue(Data[1]);
      $b=$this->getValue(Data[2]);
      $this->setValue(Data[3],$a+$b);
      break;
    case "sub":
      $a=$this->getValue(Data[1]);
      $b=$this->getValue(Data[2]);
      $this->setValue(Data[3],$a-$b);
      break;
    case "mul":
      $a=$this->getValue(Data[1]);
      $b=$this->getValue(Data[2]);
      $this->setValue(Data[3],$a*$b);
      break;
    case "div":
      $a=$this->getValue(Data[1]);
      $b=$this->getValue(Data[2]);
      $this->setValue(Data[3],(int)($a/$b));
      break;
    case "print":
      $a=$this->getValue(Data[1]);
      $this->log($a);
      break;
    case "ret":
      if(count($this->frames)==0) {
        $this->_currentPos = count($this->_codes);
        return false;
      }
      $frame = array_pop($this->frames);
      $frame["vars"][$frame["p"]]=$this->getValue(Data[1]);
      $this->_vars = $frame["vars"];
      $this->_currentPos = $frame["pos"];
      break;
    case "call":
      $params=Data;
      array_shift($params);
      $label = array_shift($params);
      $pos = $this->_labels[$label];
      $code=$this->_codes[$pos];
      $enter = $code["data"];
      $this->frames[] = array("p"=>array_pop($params),"vars"=>$this->_vars,"pos"=>$this->_currentPos,"nm"=>$label);
      $vars = array();
      for($i = 0; $i < count($params); $i++)
        $vars[$enter[$i+1]]=$this->getValue($params[$i]);
      $this->_vars = $vars;
      $this->_currentPos=$pos-1;
      break;
    }
    $this->_currentPos++;
    return count($this->_codes) > $this->_currentPos;
  }
}

class AsmDebugSession extends DebugSession {

  const THREAD_ID = 1;
  private $_breakpointId = 1000;
  private $_lang;
  private $_sourceFile;
  private $_breakPoints = array();
  private $_variableHandles = array();

  public function __construct() {
    parent::__construct();
    $this->setDebuggerLinesStartAt1(false);
    $this->setDebuggerColumnsStartAt1(false);
    $this->_lang = new VM();
    $self = $this;
    $this->_lang->logger = function($a)use(&$self){
      $self->log1($a);
      return $self->log($a);
    };
  }

  function initializeRequest(Response, $args) {
    $this->sendEvent("initialized");
    Response["body"] = isset(Response["body"]) ? Response["body"] : array();
    Response["body"]["supportsConfigurationDoneRequest"] = true;
    Response["body"]["supportsEvaluateForHovers"] = true;
    $this->sendResponse(Response);
  }

  protected function setBreakPointsRequest(Response, $args) {
    $path = $args["source"]["path"];
    $args["lines"] = isset($args["lines"]) ? $args["lines"] : array();
    $breakpoints = array();
    $codes = VM::parseFile($path)["codes"];
    for ($i = 0; $i < count($args["lines"]); $i++) {
      $l = $this->convertClientLineToDebugger($args["lines"][$i]);
      $verified = false;
      for($j = 0; $j < count($codes); $j++) {
        if ($codes[$j]["line"] >= $l) {
          $l = $codes[$j]["line"];
          $verified = true;
          break;
        }
      }
      if(!$verified && count($codes) > 0) {
        $verified = true;
        $l = $codes[count($codes) - 1]["line"];
      }
      $bp = array("verified"=>$verified, "line"=>$this->convertDebuggerLineToClient($l));
      $bp["id"] = $this->_breakpointId++;
      $breakpoints[]=$bp;
    }
    $this->_breakPoints[$path] = $breakpoints;
    Response["body"] = array("breakpoints"=>$breakpoints);
    $this->sendResponse(Response);
  }

  protected function threadsRequest(Response) {
    Response["body"] = array(
      "threads" => array(
        array("id"=>AsmDebugSession::THREAD_ID, "name"=>"thread 1")
      )
    );
    $this->sendResponse(Response);
  }

  protected function launchRequest(Response, $args) {
    $this->_sourceFile = $args["program"];
    $this->_lang->loadFile($this->_sourceFile);
    if (!isset($args["stopOnEntry"])) {
      if ($this->hitBreakPoint(Response)) return;
      $this->continueRequest(Response, array("threadId"=> AsmDebugSession::THREAD_ID));
      return;
    }
    $this->sendResponse(Response);
    $this->sendEvent("stopped",array("reason"=>"entry","threadId"=>AsmDebugSession::THREAD_ID));
  }

  // ▶ ボタンを押した時に呼ばれる
  protected function continueRequest(Response, $args) {
    while (true) {
      if (!$this->step(Response)) break 1;
      if ($this->hitBreakPoint(Response)) return;
    }
    $this->sendResponse(Response);
    $this->sendEvent("terminated");
  }

  private function step(Response) {
    if ($this->_lang->step()) {
      $this->sendResponse(Response);
      $this->sendEvent("stopped",array("reason"=>"step","threadId"=>AsmDebugSession::THREAD_ID));
      return true;
    }
    return false;
  }

  // ブレークポイントや例外が発生したらブレークする
  private function hitBreakPoint(Response) {
    // 対象のファイルのブレークポイントを取得する
    if (isset($this->_breakPoints[$this->_sourceFile])) {
      $breakpoints = $this->_breakPoints[$this->_sourceFile];
      $line = $this->_lang->getLine();
      // ブレークポイントがあれば止める
      $bps = array_filter($breakpoints,function($bp)use($line){
        return $bp["line"] === $this->convertDebuggerLineToClient($line);
      });
      
      if (count($bps) > 0) {
        $this->sendResponse(Response);
        $this->sendEvent("stopped",array("reason"=>"breakpoint","threadId"=>AsmDebugSession::THREAD_ID));
        return true;
      }
    }
    return false;
  }

  function convertDebuggerPathToClient($p) { return $p; }

  protected function stackTraceRequest(Response, $args) {
    $frames = array();
    $code = $this->_lang->getCode();
    for($i = count($this->_lang->frames) -1; $i >=0; $i--) {
      $frame = $this->_lang->frames[$i];
      $frames[] = array("id"=>$i+1,"name"=>$frame["nm"], "source"=>array("name"=>basename($this->_sourceFile),
        "path"=>$this->convertDebuggerPathToClient($this->_sourceFile),
        "sourceReference"=>0),
        "line"=>$this->convertDebuggerLineToClient($code["line"]), "column"=>0);
      $code=$this->_lang->getCode($frame["pos"]);
    }
    $frames[] = array(
      "id"=>0,"name"=>"main",
      "source"=>array(
        "name"=>basename($this->_sourceFile),
        "path"=>$this->convertDebuggerPathToClient($this->_sourceFile),
        "sourceReference"=>0
      ),
      "line"=>$this->convertDebuggerLineToClient($code["line"]),
      "column"=>0);
    $start  = $args["startFrame"] ? $args["startFrame"] : 0;
    $levels = $args["levels"] ? $args["levels"] : count($frames);
    
    Response["body"] = array(
      "stackFrames" => array_slice($frames, $start, min(count($frames), $start+$levels)),
      "totalFrames" => count($frames)
    );
    $this->sendResponse(Response);
  }

  // ステップオーバー
  protected function nextRequest(Response, $args) {
    Len = count($this->_lang->frames);
    do {
      if (!$this->step(Response)) {
        $this->sendResponse(Response);
        $this->sendEvent("terminated");
        return;
      }
      if ($this->hitBreakPoint(Response)) return;
    } while(Len < count($this->_lang->frames));
  }

  protected function stepInRequest(Response, $args) {
    if($this->step(Response)) return;
    $this->sendResponse(Response);
    $this->sendEvent("terminated");
  }

  protected function stepOutRequest(Response, $args) {
    Len = count($this->_lang->frames);
    while (true) {
      if (!$this->step(Response)) break;
      if (count($this->_lang->frames) < Len || $this->hitBreakPoint(Response)) return;
    }
    $this->sendResponse(Response);
    $this->sendEvent("terminated");
  }
  protected function scopesRequest(Response, $args) {
    $frameReference = $args["frameId"];
    $scopes = array();
    $scopes[] = array("name"=>"Local", "variablesReference"=>$this->createHandler("global_".$frameReference), "expensive"=>false);
    Response["body"] = array("scopes" => $scopes);
    $this->sendResponse(Response);
  }

  private $handeler_id=1000;
  private function createHandler($v) {
    $this->_variableHandles[$this->handeler_id] = $v;
    return $this->handeler_id++;
  }
  protected function variablesRequest(Response, $args) {
    $variables = array();
    if (isset($this->_variableHandles[$args["variablesReference"]])) {
      foreach ($this->_lang->_vars as $i=>$v) {
        $variables[] = array(
          "name" => $i,
          "type" => "integer",
          "value" => "".$v,
          "variablesReference" => 0,
        );
      }
    }
    Response["body"] = array("variables"=>$variables);
    $this->sendResponse(Response);
  }

  protected function setVariableRequest(Response, $args) {
    $this->_lang->setValue($args["name"], (int)$args["value"]);
    Response["body"] = $args;
    $this->sendResponse(Response);
  }

  protected function evaluateRequest(Response, $args) {
    Response["body"] = array(
      "result"=> $this->_lang->getValue($args["expression"])."",
      "variablesReference" => 0
    );
    $this->sendResponse(Response);
  }
}

(new AsmDebugSession())->dispatch();
*/
:- halt.
