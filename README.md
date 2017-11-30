# VS コードのデバッガサンプル(PHPバージョン)

これは簡単なアセンブラ言語を作ってそのデバッガを作るサンプルプロジェクトです。
通常は、TypeScriptでDebug Extensions を書くのですが、簡単な標準入出力で VS Code Debug Protocol を使ってデバッグ用のサーバが書けそうなので、Debug Extensions を PHP で書いてみたというものです。

![img](https://raw.githubusercontent.com/hsk/vscode-docs/ja/docs/extensions/images/example-debuggers/debug-arch.png)


デバッガを試す場合に、リロード作業が必要ないのでデバッガのデバッグをPrintfデバッグしながら開発したい場合に便利かもしれません。
また、初めてのデバッガ開発だけどどうしたら良いのかよくわからないという場合に、直接VSCodeのプロトコルを使うことを検討したい場合にも役立つかも知れません。

![img](https://raw.githubusercontent.com/hsk/vscode-debugger-asm-php/master/images/fig1.png)

以下のようなアセンブラをデバッグしてみることが出来ます:

asm.txt

```
main:   call addp 1 2 a
        print a
        ret 0

addp:   enter a b
        add a b c
        ret c
```

## デバッガの構成

- asm.txt : デバッガでデバッグできるアセンブラのソースコードです。
- package.json : VSCode の拡張の設定が書いてあります。
- src/extensions.ts : VSCodeに読み込まれる拡張です。 out/src.js に展開されます。デバッグサーバのプロセスを起動し標準入出力を使ってデバッガを動かします。ほとんど何もしません。
- dist/server.sh : デバッガのサーバは外部プロセスです。dist/server.php を呼び出し、標準エラーをファイルに吐き出したりします。
- dist/server.php : デバッガの本体です。言語機能そのものも内包しています。VSCodeのデバッグ時にプロセス起動されます。
- src/server.ts : こちらはTypeScriptによるデバッガの本体です。これは無くてもデバッガは動作します。
- client.php : デバッグサーバに接続してメッセージを送り受け取ることが出来ます。デバッガのメッセージ内容を把握するのに役立ちます。

## 実行方法

    make

とすると、このプロジェクト自体が ~/.vscode/extensions/asm-php-0.0.1 にコピーされます。

最初は一度再読込が必要です。 F1 を押した後、ウィンドウを再読込してください。

次は多分、F5 を押すと実行できると思います。

make を実行しておくとデバッグサーバの標準エラー出力が出力結果がターミナルに表示されてログとして利用できるので便利です。ただし、Unix環境でないと動かない可能性は大きいです。

デバッグコンソールに3と表示されれば成功です。

次は asm.txt を開き、ブレークポイントを設定しデバッグしてみてください。
うまく止まったりするはずです。makeによるログは特に実行時に必要ありません。

## アンインストール

    make uninstall

とすると、拡張は削除されます。

## 参考

https://github.com/hsk/vscode-docs/blob/ja/docs/extensions/example-debuggers.md
