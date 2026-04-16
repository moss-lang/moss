import { mkdirSync, statSync } from "node:fs";
import { join, resolve } from "node:path";

const workspaceRoot = resolve(import.meta.dir, "..", "..");
const clientDir = join(import.meta.dir, "web");
const buildDir = join(import.meta.dir, ".build");
const irPath = resolve(process.cwd(), process.argv[2] ?? "scratch/ir.txt");
const portEnv = process.env.PORT;
const port = portEnv == null ? 0 : Number(portEnv);

mkdirSync(buildDir, { recursive: true });

const build = await Bun.build({
  entrypoints: [join(clientDir, "main.js")],
  outdir: buildDir,
  target: "browser",
  format: "esm",
  naming: "[name].[ext]",
});

if (!build.success) {
  for (const log of build.logs) {
    console.error(log);
  }
  process.exit(1);
}

const builtMain = join(buildDir, "main.js");
const indexHtml = Bun.file(join(clientDir, "index.html"));
const styleCss = Bun.file(join(clientDir, "style.css"));

function json(data: unknown, status = 200): Response {
  return new Response(JSON.stringify(data), {
    status,
    headers: {
      "content-type": "application/json; charset=utf-8",
      "cache-control": "no-store",
    },
  });
}

function notFound(message: string): Response {
  return json({ error: message }, 404);
}

const server = Bun.serve({
  port,
  async fetch(req) {
    const url = new URL(req.url);

    if (url.pathname === "/") {
      return new Response(indexHtml, {
        headers: { "content-type": "text/html; charset=utf-8" },
      });
    }

    if (url.pathname === "/main.css") {
      return new Response(styleCss, {
        headers: { "content-type": "text/css; charset=utf-8" },
      });
    }

    if (url.pathname === "/main.js") {
      return new Response(Bun.file(builtMain), {
        headers: { "content-type": "text/javascript; charset=utf-8" },
      });
    }

    if (url.pathname === "/api/ir") {
      let stat;
      try {
        stat = statSync(irPath);
      } catch {
        return notFound(`IR dump not found: ${irPath}`);
      }

      return json({
        path: irPath,
        mtimeMs: stat.mtimeMs,
        size: stat.size,
        text: await Bun.file(irPath).text(),
      });
    }

    if (url.pathname === "/api/meta") {
      try {
        const stat = statSync(irPath);
        return json({
          path: irPath,
          mtimeMs: stat.mtimeMs,
          size: stat.size,
        });
      } catch {
        return notFound(`IR dump not found: ${irPath}`);
      }
    }

    return new Response("not found", { status: 404 });
  },
});

console.log(`IR viewer: http://localhost:${server.port}`);
console.log(`IR file:   ${irPath}`);
console.log(`cwd:       ${workspaceRoot}`);
if (portEnv == null) {
  console.log("port:      auto-selected");
}
