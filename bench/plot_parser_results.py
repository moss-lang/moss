import csv
import sys
from pathlib import Path


def read_rows(csv_path):
    rows = []
    with csv_path.open() as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append(
                {
                    "parser": row["parser"],
                    "copies": int(row["copies"]),
                    "avg_ms": float(row["avg_ms"]),
                    "nodes": int(row.get("nodes", 0)),
                }
            )
    return rows


def choose_color(idx):
    colors = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"]
    return colors[idx % len(colors)]


def write_svg(rows, png_path):
    # Simple hand-rolled SVG line chart to avoid third-party deps.
    width, height = 800, 480
    margin = 60
    min_x = min(row["copies"] for row in rows)
    max_x = max(row["copies"] for row in rows)
    max_y = max(row["avg_ms"] for row in rows)

    def sx(x):
        span = max_x - min_x if max_x != min_x else 1
        return margin + (x - min_x) / span * (width - 2 * margin)

    def sy(y):
        span = max_y if max_y != 0 else 1
        return height - margin - y / span * (height - 2 * margin)

    parsers = sorted({row["parser"] for row in rows})
    svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}">']
    svg.append('<rect width="100%" height="100%" fill="white"/>')
    # Axes
    svg.append(f'<line x1="{margin}" y1="{margin}" x2="{margin}" y2="{height - margin}" stroke="#000"/>')
    svg.append(f'<line x1="{margin}" y1="{height - margin}" x2="{width - margin}" y2="{height - margin}" stroke="#000"/>')

    # Ticks
    xticks = sorted({row["copies"] for row in rows})
    for x in xticks:
        px = sx(x)
        svg.append(f'<line x1="{px}" y1="{height - margin}" x2="{px}" y2="{height - margin + 6}" stroke="#000"/>')
        svg.append(
            f'<text x="{px}" y="{height - margin + 24}" font-size="12" text-anchor="middle" fill="#000">{x}</text>'
        )

    y_ticks = 5
    for i in range(y_ticks + 1):
        value = max_y * i / y_ticks
        py = sy(value)
        svg.append(f'<line x1="{margin - 6}" y1="{py}" x2="{margin}" y2="{py}" stroke="#000"/>')
        svg.append(
            f'<text x="{margin - 10}" y="{py + 4}" font-size="12" text-anchor="end" fill="#000">{value:.1f}</text>'
        )
        svg.append(
            f'<line x1="{margin}" y1="{py}" x2="{width - margin}" y2="{py}" stroke="#ccc" stroke-dasharray="2,4"/>'
        )

    # Series
    legend_x = width - margin - 140  # keep legend inside bounds
    legend_y_start = margin
    for idx, parser in enumerate(parsers):
        color = choose_color(idx)
        series = [row for row in rows if row["parser"] == parser]
        series.sort(key=lambda r: r["copies"])
        path_cmds = []
        for j, row in enumerate(series):
            x, y = sx(row["copies"]), sy(row["avg_ms"])
            cmd = "M" if j == 0 else "L"
            path_cmds.append(f"{cmd}{x:.2f},{y:.2f}")
            svg.append(
                f'<circle cx="{x:.2f}" cy="{y:.2f}" r="3" fill="{color}" stroke="none" />'
            )
        if path_cmds:
            svg.append(
                f'<path d="{' '.join(path_cmds)}" fill="none" stroke="{color}" stroke-width="2" />'
            )
        # Legend entry
        legend_y = legend_y_start + idx * 20
        svg.append(f'<rect x="{legend_x}" y="{legend_y - 8}" width="12" height="12" fill="{color}" />')
        svg.append(
            f'<text x="{legend_x + 18}" y="{legend_y + 2}" font-size="12" fill="#000">{parser}</text>'
        )

    svg.append(
        f'<text x="{(width)/2}" y="{height - 12}" font-size="14" text-anchor="middle"># copies of lib/wasi.moss</text>'
    )
    svg.append(
        f'<text x="15" y="{height/2}" font-size="14" text-anchor="middle" transform="rotate(-90 15,{height/2})">avg parse time (ms)</text>'
    )
    svg.append(
        f'<text x="{width/2}" y="30" font-size="16" text-anchor="middle">Parser performance comparison</text>'
    )
    svg.append("</svg>")
    png_path.write_text("\n".join(svg))
    print(f"wrote {png_path}")


def main():
    if len(sys.argv) < 3:
        print("usage: plot_parser_results.py <csv-path> <svg-path>")
        sys.exit(1)

    csv_path = Path(sys.argv[1])
    svg_path = Path(sys.argv[2])
    rows = read_rows(csv_path)
    if not rows:
        print("no data")
        return
    write_svg(rows, svg_path)


if __name__ == "__main__":
    main()
