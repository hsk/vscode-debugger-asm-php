all:
	make uninstall
	cp -rf ../vscode-debugger-simple-asm ~/.vscode/extensions/simple-asm-0.0.1
	make tail

uninstall:
	rm -rf /tmp/server.log
	rm -rf /tmp/recive.log
	rm -rf ~/.vscode/extensions/simple-asm-0.0.1
ocaml:
	cd dist/ocaml; make

tail:
	touch /tmp/server.log
	tail -100f /tmp/server.log
tail2:
	touch /tmp/recive.log
	tail -100f /tmp/recive.log
ts:
	npm run-script vscode:prepublish
	make all
client:
	php client.php -a
