"""The native side of the bootstrap Std (D38).

Everything the interpreter provides natively is declared in one place here,
so the Moss-side declarations in lib/ and the Python implementations cannot
drift apart silently: lib/char.moss is *generated* from CHARS (see
gen_char_moss), and tests assert the file matches.
"""

from string import ascii_lowercase, ascii_uppercase

CHARS = {}
for c in ascii_lowercase + ascii_uppercase:
    CHARS[c] = c
for i in range(10):
    CHARS[f"digit{i}"] = str(i)
CHARS.update(
    {
        "exclam": "!",
        "percent": "%",
        "ampersand": "&",
        "lparen": "(",
        "rparen": ")",
        "star": "*",
        "plus": "+",
        "comma": ",",
        "hyphen": "-",
        "dot": ".",
        "slash": "/",
        "colon": ":",
        "semi": ";",
        "less": "<",
        "equal": "=",
        "greater": ">",
        "lbracket": "[",
        "rbracket": "]",
        "caret": "^",
        "lbrace": "{",
        "pipe": "|",
        "rbrace": "}",
        "space": " ",
        "newline": "\n",
        "tab": "\t",
        "cr": "\r",
        "underscore": "_",
        "quote": '"',
        "apostrophe": "'",
        "octothorpe": "#",
        "backslash": "\\",
        "question": "?",
        "at": "@",
        "dollar": "$",
        "backtick": "`",
        "tilde": "~",
    }
)


def gen_char_moss() -> str:
    lines = [
        "# Generated from bootstrap/mossc/native.py; edit CHARS there and",
        "# regenerate with: python3 -c 'from mossc.native import gen_char_moss;"
        " print(gen_char_moss(), end=\"\")'",
        "",
        'import "./int.moss" use Int;',
        'import "./num.moss" use .eq, .ne, .lt, .gt, .le, .ge;',
        "",
        "type Char;",
        "",
        "assume Char {",
    ]
    for name in CHARS:
        lines.append(f"  val {name}: Char;")
    lines.append("")
    lines.append("  assume Int {")
    lines.append("    # Char <-> Int, so text can be stored as codepoints")
    lines.append("    # rather than as spans into one source buffer. Both are")
    lines.append("    # detached, so the receiver is whichever this module's")
    lines.append("    # importer provides them at: `Char.code` and `Int.char`.")
    lines.append("    fn .code(): Int;")
    lines.append("    fn .char(): Char;")
    lines.append("  }")
    lines.append("")
    lines.append("  context Chars =")
    for method in ("eq", "ne", "lt", "gt", "le", "ge"):
        lines.append(f"    Char.{method},")
    for name in CHARS:
        lines.append(f"    {name},")
    lines.append("  ;")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def gen_wasichar_moss() -> str:
    """The char constants, provided over `Wasm` instead of natively.

    There are no literals (D4), so each codepoint is an or of powers of
    two. Generated rather than written because there are ninety of them
    and they are all the same shape.
    """
    lines = [
        "# Generated from bootstrap/mossc/native.py; regenerate with:",
        "#   python3 -c 'from mossc.native import gen_wasichar_moss;"
        " print(gen_wasichar_moss(), end=\"\")'",
        "",
        'import "./wasm.moss" as w use Wasm, I32;',
        'import "./char.moss" as char use Char;',
        'import "./wasistd.moss" use Chr;',
        "",
        "assume Wasm {",
        "  # Shift amounts, built from `i32_one` like everything else.",
        "  fn n1(): I32 { w::i32_one }",
        "  fn n2(): I32 { w::i32_add(n1(), n1()) }",
        "  fn n3(): I32 { w::i32_add(n2(), n1()) }",
        "  fn n4(): I32 { w::i32_add(n2(), n2()) }",
        "  fn n5(): I32 { w::i32_add(n4(), n1()) }",
        "  fn n6(): I32 { w::i32_add(n4(), n2()) }",
        "",
        "  fn bit(k: I32): I32 { w::i32_shl(w::i32_one, k) }",
        "",
    ]

    def expr(code: int) -> str:
        bits = [k for k in range(7) if code >> k & 1]
        terms = ["w::i32_one" if k == 0 else f"bit(n{k}())" for k in bits]
        out = terms[0]
        for term in terms[1:]:
            out = f"w::i32_or({out}, {term})"
        return out

    for name, value in CHARS.items():
        lines.append(f"  fn char_{name}(): Chr {{ Chr ({expr(ord(value))}) }}")
    lines.append("}")
    lines.append("")
    lines.append("context WasiChars =")
    for name in CHARS:
        lines.append(f"  char::{name},")
    lines.append(";")
    lines.append("")
    lines.append("functor WasiCharConsts: Wasm -> WasiChars {")
    for name in CHARS:
        lines.append(f"  bind char::{name} = char_{name}();")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)
