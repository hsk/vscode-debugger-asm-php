'use strict';

import * as vscode from 'vscode';
import { WorkspaceFolder, DebugConfiguration, ProviderResult, CancellationToken } from 'vscode';
import { join } from 'path';
import { stringify } from 'querystring';

export function activate(context: vscode.ExtensionContext) {
  context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('simple-asm', new SimpleAsmConfigurationProvider()));
  context.subscriptions.push(vscode.commands.registerCommand('extension.simple-asm-debug.getProgramName', config => {
    return {command: context.extensionPath+"/dist/server.sh", args:[SimpleAsmConfigurationProvider.runtime,SimpleAsmConfigurationProvider.mode]};
  }));
}
export function deactivate() {}

class SimpleAsmConfigurationProvider implements vscode.DebugConfigurationProvider {
    static runtime = "";
    static mode="server.php";
    resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {
  
      // if launch.json is missing or empty
      if (!config.type && !config.request && !config.name) {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'text' ) {
          config.type = 'simple-asm';
          config.name = 'Launch';
          config.request = 'launch';
          config.program = '${file}';
          config.stopOnEntry = true;
        }
      }
  
      if (!config.program) {
        return vscode.window.showInformationMessage("Cannot find a program to debug").then(_ => {
          return undefined;	// abort launch
        });
      }
      SimpleAsmConfigurationProvider.mode = config.mode ? config.mode : "server.php";
      SimpleAsmConfigurationProvider.runtime = config.runtime ? config.runtime : "";
      return config;
    }
  }
  