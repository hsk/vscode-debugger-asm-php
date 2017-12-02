#!/usr/bin/php
<?php

class DebugSession {
  function __construct(){}
  private $lineAt1 = true;
  private $columnAt1 = true;
  function setDebuggerLinesStartAt1($bool) {$this->lineAt1 = $bool;}
  function setDebuggerColumnsStartAt1($bool) {$this->columnAt1 = $bool;}
  function convertClientLineToDebugger($i) {
    if($this->lineAt1) return $i; else return $i-1;
  }
  function convertDebuggerLineToClient($i) {
    if($this->columnAt1) return $i; else return $i+1;
  }
  function json_encode($arr) {
    return json_encode($arr,JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES);
  }
  function json_decode($arr) {
    return json_decode($arr,true,JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES);
  }
  private $seq=1;
  function sendJSON($arr) {
    $data=array("seq"=>$this->seq++);
    foreach($arr as $k=>$v)$data[$k]=$v;
    $str = $this->json_encode($data);
    $this->log1("<".$str);
    printf("Content-Length: %d\r\n\r\n%s", strlen($str),$str);
  }
  function sendEvent($event,$body=null) {
    $arr = array("type"=>"event","event"=>$event);
    if($body!==null) $arr["body"]=$body;
    $this->sendJSON($arr);
  }
  public $last_request_seq=0;
  function sendResponse($arr) {
    if($this->last_request_seq==$arr["request_seq"]) return;
    $this->last_request_seq=$arr["request_seq"];
    $this->sendJSON($arr);
  }
  function log($msg) {
    $this->sendEvent("output",array("category"=>"console","output"=>$msg."\n"));
  }
  function log1($str) {
    fprintf(STDERR, "%s\n", $str);
  }
  function log2($str) {
    $fp=fopen("/tmp/recive.log","a");
    fprintf($fp, "%s\n", $str);
    fclose($fp);
  }
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

  function dispatch() {
    $this->log1("**************************************************");
    while(true) {
      $data = $this->readData(STDIN);
      if($data===false) break;
      $this->log2($data);
      $this->log1($data);
      $data = $this->json_decode(trim($data));
      $command = $data["command"];
      switch($data["command"]) {
      case "initialize": case "setBreakpoints": case "setExceptionBreakpoints":
      case "configurationDone": case "disconnect": case "threads": case "launch":
      case "stackTrace": case "next": case "stepIn": case "stepOut": case "continue":
      case "scopes": case "variables": case "setVariable": case "evaluate":
        $response=array(
          "type"=>"response",
          "request_seq"=>$data["seq"],
          "command"=>$data["command"],
          "success"=>true,
        );
        $command.="Request";
        if(isset($data["body"])) $response["body"]=$data["body"];
        if(!isset($data["arguments"])) $data["arguments"]=null;
        $this->last_request_seq=0;
        call_user_func(array($this,$command),$response,$data["arguments"]);
        $this->log1("call method ok-------------------");
        if($command=="disconnectRequest") return;
        break;
      default:
        $this->log('unknown command '.$this->json_encode($data));
      }
    }
  }
  function setExceptionBreakpointsRequest($response, $argv) {
    $this->sendResponse($response);
  }
  function configurationDoneRequest($response, $argv) {
    $this->sendResponse($response);
  }
  function disconnectRequest($response, $argv) {
    $this->sendResponse($response);
    $this->log("disconnect");
    $this->log("exit process");
  }
}

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
  function frame($label, $p) {
    return array("p"=>$p, "vars"=> $this->_vars, "pos"=> $this->_currentPos, "nm"=>$label);
  }

  function loadFile($filename) {
    $a = VM::parseFile($filename);
    $this->_codes = $a["codes"];
    $this->_labels = $a["labels"];
    $this->_currentPos = $a["labels"]["main"];
    $this->frames = array($this->frame("main","a"));
  }
  function getValue($argv) {
    if(is_numeric($argv)) return (int)$argv;
    if(isset($this->_vars[$argv])) return $this->_vars[$argv];
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
    $data = $code["data"];
    switch($data[0]) {
    case "add":
      $a=$this->getValue($data[1]);
      $b=$this->getValue($data[2]);
      $this->setValue($data[3],$a+$b);
      break;
    case "sub":
      $a=$this->getValue($data[1]);
      $b=$this->getValue($data[2]);
      $this->setValue($data[3],$a-$b);
      break;
    case "mul":
      $a=$this->getValue($data[1]);
      $b=$this->getValue($data[2]);
      $this->setValue($data[3],$a*$b);
      break;
    case "div":
      $a=$this->getValue($data[1]);
      $b=$this->getValue($data[2]);
      $this->setValue($data[3],(int)($a/$b));
      break;
    case "print":
      $a=$this->getValue($data[1]);
      $this->log($a);
      break;
    case "ret":
      if(count($this->frames)<= 1) {
        $this->_currentPos = count($this->_codes);
        return false;
      }
      $frame = array_pop($this->frames);
      $frame["vars"][$frame["p"]]=$this->getValue($data[1]);
      $this->_vars = $frame["vars"];
      $this->_currentPos = $frame["pos"];
      break;
    case "call":
      $params=$data;
      array_shift($params);
      $label = array_shift($params);
      $pos = $this->_labels[$label];
      $code=$this->_codes[$pos];
      $enter = $code["data"];
      $this->frames[] = $this->frame($label,array_pop($params));
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

  function initializeRequest($response, $args) {
    $this->sendEvent("initialized");
    $response["body"] = isset($response["body"]) ? $response["body"] : array();
    $response["body"]["supportsConfigurationDoneRequest"] = true;
    $response["body"]["supportsEvaluateForHovers"] = true;
    $this->sendResponse($response);
  }

  protected function setBreakPointsRequest($response, $args) {
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
    $response["body"] = array("breakpoints"=>$breakpoints);
    $this->sendResponse($response);
  }

  protected function threadsRequest($response) {
    $response["body"] = array(
      "threads" => array(
        array("id"=>AsmDebugSession::THREAD_ID, "name"=>"thread 1")
      )
    );
    $this->sendResponse($response);
  }

  protected function launchRequest($response, $args) {
    $this->_sourceFile = $args["program"];
    $this->_lang->loadFile($this->_sourceFile);
    if (!isset($args["stopOnEntry"])) {
      if ($this->hitBreakPoint($response)) return $this->sendStoped($response,"breakpoint");
      return $this->continueRequest($response, array("threadId"=> AsmDebugSession::THREAD_ID));
    }
    $this->sendStoped($response,"entry");
  }

  // ▶ ボタンを押した時に呼ばれる
  protected function continueRequest($response, $args) {
    while (true) {
      if ($this->step()) return $this->sendTerminated($response);
      if ($this->hitBreakPoint()) return $this->sendStoped($response,"breakpoint");
    }
  }
  function sendStoped($response, $reason) {
    $this->sendResponse($response);
    $this->sendEvent("stopped",array("reason"=>$reason,"threadId"=>AsmDebugSession::THREAD_ID));
  }
  private function step() {
    return !$this->_lang->step();
  }

  // ブレークポイントや例外が発生したらブレークする
  private function hitBreakPoint() {
    // 対象のファイルのブレークポイントを取得する
    if (isset($this->_breakPoints[$this->_sourceFile])) {
      $breakpoints = $this->_breakPoints[$this->_sourceFile];
      $line = $this->_lang->getLine();
      // ブレークポイントがあれば止める
      $bps = array_filter($breakpoints,function($bp)use($line){
        return $bp["line"] === $this->convertDebuggerLineToClient($line);
      });
      return (count($bps) > 0);
    }
    return false;
  }

  function convertDebuggerPathToClient($p) { return $p; }

  protected function stackTraceRequest($response, $args) {
    $frames = array();
    $code = $this->_lang->getCode();
    for($i = count($this->_lang->frames) -1; $i >=0; $i--) {
      $frame = $this->_lang->frames[$i];
      $frames[] = array("id"=>$i,"name"=>$frame["nm"], "source"=>array("name"=>basename($this->_sourceFile),
        "path"=>$this->convertDebuggerPathToClient($this->_sourceFile),
        "sourceReference"=>0),
        "line"=>$this->convertDebuggerLineToClient($code["line"]), "column"=>0);
      $code=$this->_lang->getCode($frame["pos"]);
    }
    $start  = $args["startFrame"] ? $args["startFrame"] : 0;
    $levels = $args["levels"] ? $args["levels"] : count($frames);
    
    $response["body"] = array(
      "stackFrames" => array_slice($frames, $start, min(count($frames), $start+$levels)),
      "totalFrames" => count($frames)
    );
    $this->sendResponse($response);
  }
  function sendTerminated($response) {
    $this->sendResponse($response);
    $this->sendEvent("terminated");
  }
  // ステップオーバー
  protected function nextRequest($response, $args) {
    $len = count($this->_lang->frames);
    while (true) {
      if ($this->step()) return $this->sendTerminated($response);
      if ($this->hitBreakPoint()) return $this->sendStoped($response,"breakpoint");
      if ($len >= count($this->_lang->frames)) return $this->sendStoped($response,"step");
    }
  }

  protected function stepInRequest($response, $args) {
    if($this->step()) return $this->sendTerminated($response);
    $this->sendStoped($response,"step");
  }

  protected function stepOutRequest($response, $args) {
    $len = count($this->_lang->frames);
    while (true) {
      if ($this->step()) return $this->sendTerminated($response);
      if ($this->hitBreakPoint()) return $this->sendStoped($response,"breakpoint");
      if (count($this->_lang->frames) < $len) return $this->sendStoped($response,"step");
    }
  }
  protected function scopesRequest($response, $args) {
    $frameReference = $args["frameId"];
    $scopes = array();
    $scopes[] = array("name"=>"Local", "variablesReference"=>$this->createHandler("global_".$frameReference), "expensive"=>false);
    $response["body"] = array("scopes" => $scopes);
    $this->sendResponse($response);
  }

  private $handeler_id=1000;
  private function createHandler($v) {
    $this->_variableHandles[$this->handeler_id] = $v;
    return $this->handeler_id++;
  }
  protected function variablesRequest($response, $args) {
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
    $response["body"] = array("variables"=>$variables);
    $this->sendResponse($response);
  }

  protected function setVariableRequest($response, $args) {
    $this->_lang->setValue($args["name"], (int)$args["value"]);
    $response["body"] = $args;
    $this->sendResponse($response);
  }

  protected function evaluateRequest($response, $args) {
    $response["body"] = array(
      "result"=> $this->_lang->getValue($args["expression"])."",
      "variablesReference" => 0
    );
    $this->sendResponse($response);
  }
}

(new AsmDebugSession())->dispatch();
