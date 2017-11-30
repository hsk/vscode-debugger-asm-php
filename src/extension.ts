'use strict';
import * as vscode from 'vscode';


export function activate(context: vscode.ExtensionContext) {

    // 構成の追加用コマンドの追加と、削除設定
	context.subscriptions.push(vscode.commands.registerCommand('extension.asm-debug.provideInitialConfigurations', () => {
        // デバッグの構成の追加メニューから ./vscode/launch.json に設定を作る
        const initialConfigurations = {
            version: '0.2.0',
            configurations: [
                {
                    type: 'asm',
                    request: 'launch',
                    name: 'Asm Debug',
                    program: '${workspaceRoot}/asm.txt',
                    stopOnEntry: true
                }
            ]
        };
		return [
			'// Asm 拡張 (src/extension.ts) から作った！！',
			'// Hover to view descriptions of existing attributes.',
			JSON.stringify(initialConfigurations, null, '\t')
		].join('\n');
	}));

}
export function deactivate() {}
