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

async function renderSvg(dot: string): Promise<Response> {
  const proc = Bun.spawn(["dot", "-Tsvg"], {
    stdin: new Blob([dot]),
    stdout: "pipe",
    stderr: "pipe",
  });

  const [stdout, stderr, exitCode] = await Promise.all([
    new Response(proc.stdout).text(),
    new Response(proc.stderr).text(),
    proc.exited,
  ]);

  if (exitCode !== 0) {
    return json(
      {
        error: stderr.trim() || `dot exited with code ${exitCode}`,
      },
      500,
    );
  }

  return new Response(stdout, {
    headers: {
      "content-type": "image/svg+xml; charset=utf-8",
      "cache-control": "no-store",
    },
  });
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

    if (url.pathname === "/api/render-svg" && req.method === "POST") {
      let payload;
      try {
        payload = await req.json();
      } catch {
        return json({ error: "invalid JSON request body" }, 400);
      }

      if (typeof payload?.dot !== "string" || payload.dot.length === 0) {
        return json({ error: "request body must include non-empty `dot`" }, 400);
      }

      return renderSvg(payload.dot);
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
