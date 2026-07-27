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
        "type Char;",
        "",
        "assume Char {",
    ]
    for name in CHARS:
        lines.append(f"  val {name}: Char;")
    lines.append("")
    lines.append("  context Chars =")
    for name in CHARS:
        lines.append(f"    {name},")
    lines.append("  ;")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)
