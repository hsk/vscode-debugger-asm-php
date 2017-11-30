<?php
if(isset($argv[1]) && $argv[1]=="-s") {
  server();
} else {
  $server = 'php client.php -s';
  if(isset($argv[1]))$server=$argv[1];
  if(isset($argv[1]) && $argv[1]=="-p")$server="./dist/server.sh";
  if(isset($argv[1]) && $argv[1]=="-j")$server="node out/src/server.js";
  client($server);
}

function readData($fp) {
  stream_set_blocking ($fp , false );
  usleep(1000*10);
  $len = 0;
  $data = "";
  while(!feof($fp)) {
    $stdin = fgets($fp);
    if ($stdin === false) return false;
    $stdin = trim($stdin);
    if ($stdin === '') break;
    if (preg_match('/^Content-Length: (\\d+)/',$stdin, $m)>0) $len = $m[1];
  }
  if($len <= 0) return false;
  return fread($fp,$len);
}
function sendMessage($fp,$data) {
  fprintf($fp, "Content-Length: %d\r\n\r\n%s", strlen($data),$data);
}

function client($server) {
  $desc = array(
      array('pipe', 'r'),
      array('pipe', 'w'),
  );
  $proc = proc_open($server, $desc, $pipes);
  while(!feof($pipes[1])){
    $data = trim(fgets(STDIN));
    if($data != "")
      fprintf($pipes[0],"Content-Length: %d\r\n\r\n%s",strlen($data),$data);
    while(true){
      $rc = readData($pipes[1]);
      if($rc === false) break;
      printf("<[%s]\n",$rc);
    }
    printf("call method ok-------------------\n");
  }
  fclose($pipes[0]);
  fclose($pipes[1]);
  proc_close($proc);
}

function server() {
  $fp=fopen("server.log","a");
  fprintf($fp,"listen\n");
  while(true) {
      $data = readData(STDIN);
      if($data===false) break;
      fprintf($fp, "read %s\n",$data);
      $data = json_decode(trim($data),true);
      $str = var_export($data,true);
      sendMessage(STDOUT,$str);
  }
  fprintf($fp, "end\n");
  fclose($fp);
}

