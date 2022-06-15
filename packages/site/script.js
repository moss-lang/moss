import helloExample from "@moss-lang/examples/hello.moss?raw";
import * as lsp from "@moss-lang/lsp";
import * as monaco from "monaco-editor";
import {
  AbstractMessageReader,
  AbstractMessageWriter,
  Disposable,
  MonacoLanguageClient,
  MonacoServices,
} from "monaco-languageclient";
import language from "tree-sitter-moss/tree-sitter-moss.wasm?url";
import Parser from "web-tree-sitter";

// https://github.com/TypeFox/monaco-languageclient/blob/v1.0.1/packages/examples/client/src/client.ts

monaco.languages.register({ id: "moss", extensions: [".moss"] });

monaco.editor.create(document.getElementById("container"), {
  model: monaco.editor.createModel(
    helloExample,
    "moss",
    monaco.Uri.parse("inmemory://model.moss")
  ),
  "semanticHighlighting.enabled": true,
  theme: "vs-dark",
});

MonacoServices.install(monaco);

class DirectMessageReader extends AbstractMessageReader {
  constructor() {
    super();
    this.callbacks = [];
  }

  listen(callback) {
    const i = this.callbacks.length;
    this.callbacks.push(callback);
    return Disposable.create(() => {
      delete this.callbacks[i];
    });
  }
}

class DirectMessageWriter extends AbstractMessageWriter {
  constructor(reader) {
    super();
    this.reader = reader;
  }

  async write(msg) {
    this.reader.callbacks.forEach((callback) => callback(msg));
  }

  end() {}
}

const makeChannel = () => {
  const reader = new DirectMessageReader();
  const writer = new DirectMessageWriter(reader);
  return { reader, writer };
};

const channel1 = makeChannel();
const channel2 = makeChannel();

lsp.startServer({
  makeParser: async () => {
    await Parser.init();
    const parser = new Parser();
    parser.setLanguage(await Parser.Language.load(language));
    return parser;
  },
  reader: channel1.reader,
  writer: channel2.writer,
});

new MonacoLanguageClient({
  name: "Moss Language Client",
  clientOptions: {
    // use a language id as a document selector
    documentSelector: ["moss"],
    // disable the default error handler
    errorHandler: {
      error: () => ({ action: ErrorAction.Continue }),
      closed: () => ({ action: CloseAction.DoNotRestart }),
    },
  },
  connectionProvider: {
    get: async () => ({ reader: channel2.reader, writer: channel1.writer }),
  },
}).start();
