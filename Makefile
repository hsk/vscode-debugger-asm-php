all:
	rm -rf ~/.vscode/extensions/asm-php-0.0.1
	cp -rf ../vscode-debugger-asm-php ~/.vscode/extensions/asm-php-0.0.1
	make tail
all2:
	rm -rf /tmp/server.log
	rm -rf /tmp/recive.log
	make all
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
