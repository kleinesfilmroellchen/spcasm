/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
  languages,
  workspace,
  EventEmitter,
  ExtensionContext,
  window,
  InlayHintsProvider,
  TextDocument,
  CancellationToken,
  Range,
  InlayHint,
  TextDocumentChangeEvent,
  ProviderResult,
  commands,
  WorkspaceEdit,
  TextEdit,
  Selection,
  Uri,
  OutputChannel,
} from "vscode";

import {
  Disposable,
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
let context: ExtensionContext;
let traceOutputChannel: OutputChannel;
let clientOutputChannel: OutputChannel;

export async function activate(providedContext: ExtensionContext) {
  commands.registerCommand("sals.restart", restartServer);
  context = providedContext;
  traceOutputChannel = window.createOutputChannel(
    "sals Language Server trace",
    "spc700"
  );
  clientOutputChannel = window.createOutputChannel(
    "sals Language Client",
    "spc700"
  );
  startServer();
}

function startServer() {
  const settings = workspace.getConfiguration("sals");
  const packagedSalsPath = (settings.get("executable.override") as boolean)
    ? (settings.get("executable.path") as string)
    : context.extensionPath + "/dist/bin/sals";
  const command = process.env.SERVER_PATH || packagedSalsPath;
  const run: Executable = {
    command,
    options: {
      env: {
        ...process.env,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        RUST_LOG: "debug",
      },
    },
  };
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  // Options to control the language client.
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "spc700" }],
    traceOutputChannel,
    outputChannel: clientOutputChannel,
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "sals",
    "sals Language Server",
    serverOptions,
    clientOptions
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) return undefined;

  return client.stop();
}

export async function restartServer(
  _arguments: any[]
): Promise<Thenable<void> | undefined> {
  if (!client) {
    startServer();
  } else {
    await client.stop();
    startServer();
  }
}
