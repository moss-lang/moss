import * as process from "node:process";
import * as vscode from "vscode";
import * as lsp from "vscode-languageclient/node";

const exeDefault = (context: vscode.ExtensionContext): string => {
  const uri = context.extensionUri;
  const ext = process.platform === "win32" ? ".exe" : "";
  return vscode.Uri.joinPath(uri, "bin", `connie${ext}`).fsPath;
};

let client: lsp.LanguageClient;

export const activate = (context: vscode.ExtensionContext): void => {
  const command =
    vscode.workspace.getConfiguration("connie").get<string>("exe") ??
    exeDefault(context);
  client = new lsp.LanguageClient(
    "connie",
    "Connie",
    { command, args: ["lsp"] },
    { documentSelector: ["connie"] }
  );
  client.start();
};

export const deactivate = (): void => {
  if (client) client.stop();
};
