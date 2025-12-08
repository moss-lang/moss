use std::{
    fs,
    hint::black_box,
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, Instant},
};

use moss_core::{
    lex::{Tokens, lex},
    parse, parse_naive,
};

const RUNS: usize = 1000;
const COPIES: &[usize] = &[1, 2, 4, 8, 16, 32, 64, 128, 256, 512];

#[derive(Debug)]
struct Row {
    parser: &'static str,
    copies: usize,
    avg_ms: f64,
    nodes: usize,
}

fn bench_rust_parser<F>(inputs: &[(usize, String)], name: &'static str, mut parser: F) -> Vec<Row>
where
    F: FnMut(&Tokens) -> Result<usize, String>,
{
    let mut rows = Vec::new();
    for (copies, source) in inputs {
        let mut total = Duration::ZERO;
        let mut last_nodes = 0usize;
        let mut checksum = 0usize;
        for _ in 0..RUNS {
            let start = Instant::now();
            let (tokens, _) = lex(source).expect("lex");
            let nodes = parser(&tokens).expect("parse");
            checksum = checksum.wrapping_add(nodes);
            last_nodes = nodes;
            total += start.elapsed();
            black_box(nodes);
        }
        rows.push(Row {
            parser: name,
            copies: *copies,
            avg_ms: total.as_secs_f64() * 1000.0 / RUNS as f64,
            nodes: last_nodes,
        });
        eprintln!("{} copies {} checksum {}", name, copies, checksum);
    }
    rows
}

fn ensure_java(workspace_root: &Path) {
    let status = Command::new("javac")
        .current_dir(workspace_root)
        .args(["-d", "bench/java", "bench/java/ParserBench.java"])
        .status()
        .expect("failed to invoke javac");
    assert!(status.success(), "javac failed");
}

fn bench_java(inputs: &[(usize, String)], workspace_root: &Path, base_path: &Path) -> Vec<Row> {
    ensure_java(workspace_root);
    let mut rows = Vec::new();
    let base = base_path
        .canonicalize()
        .unwrap_or_else(|_| base_path.to_path_buf());
    for (copies, _) in inputs {
        let output = Command::new("java")
            .current_dir(workspace_root)
            .args([
                "-cp",
                "bench/java",
                "ParserBench",
                base.to_str().unwrap(),
                &copies.to_string(),
                &RUNS.to_string(),
            ])
            .output()
            .expect("failed to invoke java");
        assert!(output.status.success(), "java run failed");
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mut parts = stdout.trim().split_whitespace();
        let avg_ms: f64 = parts
            .next()
            .expect("missing avg")
            .parse()
            .expect("java avg parse");
        let nodes: usize = parts
            .next()
            .expect("missing nodes")
            .parse()
            .expect("java nodes parse");
        rows.push(Row {
            parser: "java-gc",
            copies: *copies,
            avg_ms,
            nodes,
        });
    }
    rows
}

fn count_tree_arena(tree: &parse::Tree) -> usize {
    tree.names.len()
        + tree.methods.len()
        + tree.types.len()
        + tree.needs.len()
        + tree.binds.len()
        + tree.params.len()
        + tree.fields.len()
        + tree.exprs.len()
        + tree.stmts.len()
        + tree.imports.len()
        + tree.tydefs.len()
        + tree.fndefs.len()
        + tree.valdefs.len()
        + tree.ctxdefs.len()
        + tree.structdefs.len()
}

fn count_path_naive(path: &parse_naive::Path) -> usize {
    1 + path.prefix.len()
}

fn count_expr_naive(expr: &parse_naive::Expr) -> usize {
    use parse_naive::Expr::*;
    match expr {
        This(_) | Int(_) | String(_) => 1,
        Path(p) => 1 + count_path_naive(p),
        Struct(p, fields) => {
            1 + count_path_naive(p) + fields.iter().map(count_field_naive).sum::<usize>()
        }
        Field(obj, _) => 1 + count_expr_naive(obj),
        Method(obj, _, args) => {
            1 + count_expr_naive(obj) + args.iter().map(count_expr_naive).sum::<usize>()
        }
        Call(p, binds, args) => {
            1 + count_path_naive(p)
                + binds.iter().map(count_bind_naive).sum::<usize>()
                + args.iter().map(count_expr_naive).sum::<usize>()
        }
        Binary(lhs, _, rhs) => 1 + count_expr_naive(lhs) + count_expr_naive(rhs),
        If(cond, then, els) => {
            1 + count_expr_naive(cond)
                + count_block_naive(then)
                + els.as_ref().map(|b| count_block_naive(b)).unwrap_or(0)
        }
    }
}

fn count_stmt_naive(stmt: &parse_naive::Stmt) -> usize {
    use parse_naive::Stmt::*;
    match stmt {
        Let(_, e) | Var(_, e) => 1 + count_expr_naive(e),
        Assign(lhs, rhs) => 1 + count_expr_naive(lhs) + count_expr_naive(rhs),
        While(cond, body) => 1 + count_expr_naive(cond) + count_block_naive(body),
        Expr(e) => 1 + count_expr_naive(e),
    }
}

fn count_block_naive(block: &parse_naive::Block) -> usize {
    1 + block.stmts.iter().map(count_stmt_naive).sum::<usize>()
        + block
            .expr
            .as_ref()
            .map(|e| count_expr_naive(e))
            .unwrap_or(0)
}

fn count_need_naive(need: &parse_naive::Need) -> usize {
    1 + count_path_naive(&need.path)
}

fn count_bind_naive(bind: &parse_naive::Bind) -> usize {
    1 + count_path_naive(&bind.path) + count_expr_naive(&bind.val)
}

fn count_param_naive(param: &parse_naive::Param) -> usize {
    1 + count_path_naive(match &param.ty {
        parse_naive::Type::Path(p) => p,
    })
}

fn count_field_naive(field: &parse_naive::Field) -> usize {
    1 + count_expr_naive(&field.val)
}

fn count_tree_naive(tree: &parse_naive::Tree) -> usize {
    let mut total = 0;
    total += tree.imports.len();
    total += tree.tydefs.len();
    total += tree.fndefs.len();
    total += tree.valdefs.len();
    total += tree.ctxdefs.len();
    total += tree.structdefs.len();

    for import in &tree.imports {
        total += import.names.len();
        total += import.methods.len();
    }
    for tydef in &tree.tydefs {
        if let Some(parse_naive::Type::Path(p)) = &tydef.def {
            total += count_path_naive(p);
        }
    }
    for fndef in &tree.fndefs {
        total += fndef.needs.iter().map(count_need_naive).sum::<usize>();
        total += fndef.params.iter().map(count_param_naive).sum::<usize>();
        if let Some(parse_naive::Type::Path(p)) = &fndef.result {
            total += count_path_naive(p);
        }
        if let Some(block) = &fndef.def {
            total += count_block_naive(block);
        }
    }
    for valdef in &tree.valdefs {
        total += valdef.needs.iter().map(count_need_naive).sum::<usize>();
        total += count_path_naive(match &valdef.ty {
            parse_naive::Type::Path(p) => p,
        });
    }
    for ctxdef in &tree.ctxdefs {
        total += ctxdef.def.iter().map(count_need_naive).sum::<usize>();
    }
    for structdef in &tree.structdefs {
        total += structdef
            .fields
            .iter()
            .map(count_param_naive)
            .sum::<usize>();
    }

    total
}

fn main() {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let base_path = workspace_root.join("lib/wasi.moss");
    let source = fs::read_to_string(&base_path).expect("read lib/wasi.moss");

    let inputs: Vec<(usize, String)> = COPIES
        .iter()
        .map(|copies| (*copies, source.repeat(*copies)))
        .collect();

    let mut rows = Vec::new();

    rows.extend(bench_rust_parser(&inputs, "rust-arena", |tokens| {
        parse::parse(tokens)
            .map(|tree| count_tree_arena(&tree))
            .map_err(|e| e.message())
    }));

    rows.extend(bench_rust_parser(&inputs, "rust-naive", |tokens| {
        parse_naive::parse(tokens)
            .map(|tree| count_tree_naive(&tree))
            .map_err(|e| e.message())
    }));

    rows.extend(bench_java(&inputs, &workspace_root, &base_path));

    fs::create_dir_all(workspace_root.join("bench")).expect("create bench dir");
    let csv_path = workspace_root.join("bench/parser_bench_results.csv");
    let mut wtr = String::from("parser,copies,avg_ms,nodes\n");
    for row in &rows {
        wtr.push_str(&format!(
            "{},{},{},{}\n",
            row.parser, row.copies, row.avg_ms, row.nodes
        ));
    }
    fs::write(&csv_path, wtr).expect("write csv");

    for row in rows {
        println!("{},{},{},{}", row.parser, row.copies, row.avg_ms, row.nodes);
    }

    eprintln!("wrote {}", csv_path.display());
}
