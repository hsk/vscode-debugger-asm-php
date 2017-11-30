# VS コードのデバッガサンプル(PHPバージョン)

これはタイプスクリプトで作っていたデバッガをPHPに移植してみた時のログである。
故にドキュメントはごちゃごちゃである。
しかしとりあえず公開しておくことにするｗ

```
npm install
code .
```

F5 で、 Extension をデバッグ起動すると、このデバッグ拡張機能入りのVS Code が起動します。

起動したVS Code では test フォルダを開いて デバッグしてみることが出来ます。
最初はデバッグ起動オプションがないので、設定に追加する必要があります。

拡張の本体は、 package.jsonと src/extension.ts です。

extension.ts では本当になにもしてません。

package.jsonにデバッガの設定が書いてあります。

# 初めての通信の結果

```
make
```

で、このディレクトリを拡張に登録し、F1 でウィンドウの再起動コマンドを実行します。

次に、デバッガの構成を追加を選択し、asm-phpを登録します。

Spawn EACCES

なエラー。これは、PHPファイルに起動のアレがアレなのでchmod 755 します。

```
$ tail -100f /tmp/server.log
```

デバッガを実行してみます。

```
$ tail -100f /tmp/server.log
listen
read {"command":"initialize","arguments":{"clientID":"vscode","adapterID":"asm-php","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"ja"},"type":"request","seq":1}
end
listen
read {"command":"launch","arguments":{"type":"asm-php","request":"launch","name":"asm-phpデバッグサーバ","program":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt","__sessionId":"e9139b69-e656-4728-81d0-8f3e4768e72a"},"type":"request","seq":2}
end
```

とりあえずこんなことが言われました。

```
{"command":"initialize",
  "arguments":{
    "clientID":"vscode",
    "adapterID":"asm-php",
    "pathFormat":"path",
    "linesStartAt1":true,
    "columnsStartAt1":true,
    "supportsVariableType":true,
    "supportsVariablePaging":true,
    "supportsRunInTerminalRequest":true,
    "locale":"ja"
  },
  "type":"request",
  "seq":1
}
```

まぁ、初期化だろうな。ここで固まるのでこれをまず調べないと先に進みません。

```
{"command":"launch",
  "arguments":{
    "type":"asm-php",
    "request":"launch",
    "name":"asm-phpデバッグサーバ",
    "program":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt",
    "__sessionId":"e9139b69-e656-4728-81d0-8f3e4768e72a"},
    "type":"request",
    "seq":2
}
```

これは次の起動なのかなおそらく。プログラムのasm.txtを起動だっって言ってます。

とりあえず、initializeとlaunchとのコマンドがよばれるので、そのコマンドに対応できるようディスパッチしてみました。
で、返す改行コードは`\r\n`にして`ok` 返すとエラーメッセージがoってトークンは知らないと言われました。
これはおそらく、JSON返せば良いんじゃということで、JSONを返してみました。`{command:"ok"}`
こんどはcが駄目と。文字列か`{"command":"ok"}`
また、反応がない。JSONを返せば良いようだけど、その返し方に問題があるとかだろう。

```
  protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
    this.sendEvent(new InitializedEvent());

    response.body = response.body || {};
    response.body.supportsConfigurationDoneRequest = true;
    response.body.supportsEvaluateForHovers = true;

    this.sendResponse(response);
  }
```

こんなことをすればよいらしい。
でもわからない。
telnetで繋いでみようｗ

```
Content-Length: 277

{"command":"initialize","arguments":{"clientID":"vscode","adapterID":"asm-php","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"ja"},"type":"request","seq":1}
```
をtelnetで送るとかすればいいはず。TELネットカー。クライアントで接続してみよう。
いじいじ。ってことで、

return '{"seq":1,"type":"event","event":"initialized"}'

こんな回答を得た。



```
{"command":"initialize","arguments":{"clientID":"vscode","adapterID":"asm-php","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"ja"},"type":"request","seq":1}
return '{"seq":1,"type":"event","event":"initialized"}'
{"command":"setBreakpoints","arguments":{"source":{"name":"asm.txt","path":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt"},"lines":[2],"breakpoints":[{"line":2}],"sourceModified":false},"type":"request","seq":2}
return '{"seq":2,"type":"response","request_seq":1,"command":"initialize","success":true,"body":{"supportsConfigurationDoneRequest":true,"supportsEvaluateForHovers":true}}'
{"command":"launch","arguments":{"type":"asm-php","request":"launch","name":"asm-phpデバッグサーバ","program":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt","__sessionId":"67a97b29-6335-4fc0-8701-770ec157c856"},"type":"request","seq":3}
return '{"seq":3,"type":"response","request_seq":2,"command":"setBreakpoints","success":true,"body":{"breakpoints":[{"verified":true,"line":3,"id":1000}]}}'
{"command":"setExceptionBreakpoints","arguments":{"filters":["uncaught"]},"type":"request","seq":4}
{"command":"threads","type":"request","seq":5}
return '{"seq":4,"type":"response","request_seq":3,"command":"launch","success":true}'
return '{"seq":5,"type":"event","event":"stopped","body":{"reason":"breakpoint","threadId":1}}'
{"command":"disconnect","arguments":{"restart":false},"type":"request","seq":6}
return '{"seq":6,"type":"response","request_seq":4,"command":"setExceptionBreakpoints","success":true}'

return '{"seq":7,"type":"response","request_seq":5,"command":"threads","success":true,"body":{"threads":[{"id":1,"name":"thread 1"}]}}'

return '{"seq":8,"type":"response","request_seq":6,"command":"disconnect","success":true}'
```

色々いじって、デバッガのデバッグは順調に進んだ。
標準エラーは間にシェルスクリプトを噛ませて、ファイルにリダイレクトするようにした。
シェルスクリプトの内容を書き換えて元のtsファイルのプログラムも動くようにした。
これが幸いして、再起動レスでデバッグができるようになった。


次にするべきは、おそらく、行変換のチェックだな。
クライアントから送られてくる、行データは1行目から始まるのだけど、
内部ではゼロから始まる。
問題ない。

次はブレークポイントの受け渡しで、ランチしたときに上書きして消してた。。。
受け渡した後は、ブレークポイントチェックで、フィルタの処理のリターン書き忘れてた。
ってところを直したら、スタックトーレス要求が来た！

          case "stackTrace":
          case "next":
          case "stepIn":
          case "stepOut":
          case "continue":

あたりを次々デバッグして完了。次がscopesであった。

          case "scopes":

いつものように、client.php -j で流し込んでみれば良い。

```
{"command":"initialize","arguments":{"clientID":"vscode","adapterID":"asm-php","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"ja"},"type":"request","seq":1}
return '{"seq":1,"type":"event","event":"initialized"}'

{"command":"setBreakpoints","arguments":{"source":{"name":"asm.txt","path":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt"},"lines":[3],"breakpoints":[{"line":3}],"sourceModified":false},"type":"request","seq":2}

return '{"seq":2,"type":"response","request_seq":1,"command":"initialize","success":true,"body":{"supportsConfigurationDoneRequest":true,"supportsEvaluateForHovers":true}}'
{"command":"launch","arguments":{"type":"asm-php","request":"launch","name":"asm-phpデバッグサーバ","program":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt","__sessionId":"2a0e1748-74a9-4a31-8c25-3ad3a7d66ebc"},"type":"request","seq":3}
return '{"seq":3,"type":"response","request_seq":2,"command":"setBreakpoints","success":true,"body":{"breakpoints":[{"verified":true,"line":3,"id":1000}]}}'
attempt to send more than one response for command launch
{"command":"setExceptionBreakpoints","arguments":{"filters":["uncaught"]},"type":"request","seq":4}
return '{"seq":4,"type":"event","event":"output","body":{"category":"console","output":"continueRequest\n"}}'
{"command":"threads","type":"request","seq":5}
return '{"seq":5,"type":"event","event":"output","body":{"category":"console","output":"line:2\n"}}'
{"command":"threads","type":"request","seq":6}
return '{"seq":6,"type":"response","request_seq":3,"command":"launch","success":true}'
{"command":"configurationDone","type":"request","seq":7}
return '{"seq":7,"type":"event","event":"stopped","body":{"reason":"step","threadId":1}}'
{"command":"threads","type":"request","seq":8}
return '{"seq":8,"type":"event","event":"stopped","body":{"reason":"breakpoint","threadId":1}}'
{"command":"stackTrace","arguments":{"threadId":1,"startFrame":0,"levels":20},"type":"request","seq":9}
return '{"seq":9,"type":"event","event":"output","body":{"category":"console","output":"hitBreak\n"}}'
{"command":"stackTrace","arguments":{"threadId":1,"startFrame":0,"levels":20},"type":"request","seq":10}
return '{"seq":10,"type":"response","request_seq":4,"command":"setExceptionBreakpoints","success":true}'
{"command":"scopes","arguments":{"frameId":0},"type":"request","seq":11}


return '{"seq":11,"type":"response","request_seq":5,"command":"threads","success":true,"body":{"threads":[{"id":1,"name":"thread 1"}]}}'
return '{"seq":12,"type":"response","request_seq":6,"command":"threads","success":true,"body":{"threads":[{"id":1,"name":"thread 1"}]}}'
return '{"seq":13,"type":"response","request_seq":7,"command":"configurationDone","success":true}'

return '{"seq":14,"type":"response","request_seq":8,"command":"threads","success":true,"body":{"threads":[{"id":1,"name":"thread 1"}]}}'

return '{"seq":15,"type":"response","request_seq":9,"command":"stackTrace","success":true,"body":{"stackFrames":[{"id":0,"source":{"name":"asm.txt","path":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt","sourceReference":0},"line":3,"column":0,"name":"main"}],"totalFrames":1}}'

return '{"seq":16,"type":"response","request_seq":10,"command":"stackTrace","success":true,"body":{"stackFrames":[{"id":0,"source":{"name":"asm.txt","path":"/home/sakurai/git/vscode_lab/php/asm-php/asm.txt","sourceReference":0},"line":3,"column":0,"name":"main"}],"totalFrames":1}}'

return '{"seq":17,"type":"response","request_seq":11,"command":"scopes","success":true,"body":{"scopes":[{"name":"Local","variablesReference":1000,"expensive":false}]}}'
```

うーむ。最後悩んだのは、variablesReferenceの値に名前を入れてしまっていたことだった。
IDからの連想配列にして、IDを更新するようにして対応した。

とにかく、出来たのでリファクタリングした。
このドキュメントはわけがわからないこと間違いないので書き換える必要があるｗ
