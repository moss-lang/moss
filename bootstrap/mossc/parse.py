"""Recursive-descent parser for the grammar in docs/reference/syntax.md."""

from . import ast
from .lex import Kind, Token, lex, position


# D68: the binary operators, by the token that spells them. Precedence is
# a partial order rather than a ladder, so these are families, and the
# parser below says which families may meet without parentheses.
BINARY = {
    Kind.STAR: "*",
    Kind.SLASH: "/",
    Kind.PERCENT: "%",
    Kind.PLUS: "+",
    Kind.HYPHEN: "-",
    Kind.LESS_LESS: "<<",
    Kind.GREATER_GREATER: ">>",
    Kind.EQUAL_EQUAL: "==",
    Kind.EXCLAM_EQUAL: "!=",
    Kind.LESS: "<",
    Kind.GREATER: ">",
    Kind.LESS_EQUAL: "<=",
    Kind.GREATER_EQUAL: ">=",
    Kind.AMPERSAND: "&",
    Kind.CARET: "^",
    Kind.PIPE: "|",
}
UNARY = {Kind.HYPHEN: "-", Kind.EXCLAM: "!"}
FRACTION = ("/", "%")
ADDITIVE = ("+", "-")
SHIFT = ("<<", ">>")
COMPARISON = ("==", "!=", "<", ">", "<=", ">=")
BITWISE = ("&", "^", "|")


class ParseError(Exception):
    def __init__(self, message: str, token: Token):
        super().__init__(message)
        self.message = message
        self.token = token


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.i = 0

    # Token plumbing

    def peek(self, ahead: int = 0) -> Token:
        return self.tokens[min(self.i + ahead, len(self.tokens) - 1)]

    def at(self, kind: Kind, ahead: int = 0) -> bool:
        return self.peek(ahead).kind == kind

    def next(self) -> Token:
        token = self.peek()
        if token.kind != Kind.EOF:
            self.i += 1
        return token

    def eat(self, kind: Kind) -> Token | None:
        if self.at(kind):
            return self.next()
        return None

    def expect(self, kind: Kind) -> Token:
        if not self.at(kind):
            raise ParseError(f"expected {kind.name}, found {self.peek().kind.name}", self.peek())
        return self.next()

    def comma_list(self, parse_item, *closers: Kind) -> list:
        """List[X]: comma-separated, optional trailing comma."""
        items = []
        while not self.at_any(*closers):
            items.append(parse_item())
            if not self.eat(Kind.COMMA):
                break
        return items

    def at_any(self, *kinds: Kind) -> bool:
        return self.peek().kind in kinds

    # Names and applications

    def parse_path(self) -> ast.Path:
        path = [self.expect(Kind.NAME).text]
        while self.at(Kind.COLON_COLON):
            self.next()
            path.append(self.expect(Kind.NAME).text)
        return path

    def parse_spec(self) -> ast.Spec:
        start = self.peek().offset
        path = None
        dot = None
        if self.at(Kind.DOT):
            self.next()
            dot = self.expect(Kind.NAME).text
        else:
            path = self.parse_path()
            if self.eat(Kind.DOT):
                dot = self.expect(Kind.NAME).text
        app = self.parse_app() if self.at(Kind.LBRACKET) else None
        return ast.Spec(path, dot, app, offset=start)

    def parse_app(self) -> list[ast.Binding]:
        self.expect(Kind.LBRACKET)
        bindings = self.comma_list(self.parse_binding, Kind.RBRACKET)
        self.expect(Kind.RBRACKET)
        return bindings

    def parse_binding(self) -> ast.Binding:
        path = self.parse_path()
        self.expect(Kind.EQUAL)
        return ast.Binding(path, self.parse_spec())

    def parse_assume_item(self) -> ast.AssumeItem:
        start = self.peek().offset
        path = self.parse_path()
        dot = None
        if self.eat(Kind.DOT):
            dot = self.expect(Kind.NAME).text
        return ast.AssumeItem(path, dot, offset=start)

    # Types

    def parse_type(self) -> ast.Type:
        start = self.peek().offset
        members = []
        if self.eat(Kind.PIPE):
            if not self.at_type_atom():
                return ast.TyNever(offset=start)
        members.append(self.parse_type_atom())
        while self.eat(Kind.PIPE):
            members.append(self.parse_type_atom())
        if len(members) == 1:
            return members[0]
        return ast.TyUnion(members, offset=start)

    def at_type_atom(self) -> bool:
        return self.at_any(Kind.NAME, Kind.THIS_TYPE, Kind.LPAREN, Kind.LBRACE)

    def parse_type_atom(self) -> ast.Type:
        start = self.peek().offset
        if self.at(Kind.NAME):
            path = self.parse_path()
            app = self.parse_app() if self.at(Kind.LBRACKET) else None
            return ast.TyRef(path, app, offset=start)
        if self.eat(Kind.THIS_TYPE):
            return ast.TyThis(offset=start)
        if self.eat(Kind.LPAREN):
            if self.eat(Kind.RPAREN):
                return ast.TyUnit(offset=start)
            inner = self.parse_type()
            if self.at(Kind.COMMA):
                raise ParseError("there are no tuple types (D63); use a record", self.peek())
            self.expect(Kind.RPAREN)
            return inner  # grouping
        if self.at(Kind.LBRACE):
            return self.parse_record_type()
        raise ParseError(f"expected a type, found {self.peek().kind.name}", self.peek())

    def parse_record_type(self) -> ast.TyRecord:
        start = self.peek().offset
        self.expect(Kind.LBRACE)

        def field() -> tuple[str, ast.Type]:
            name = self.expect(Kind.NAME).text
            self.expect(Kind.COLON)
            return name, self.parse_type()

        fields = self.comma_list(field, Kind.RBRACE)
        self.expect(Kind.RBRACE)
        return ast.TyRecord(fields, offset=start)

    # Files and declarations

    def parse_file(self) -> ast.File:
        imports = []
        while self.at(Kind.IMPORT):
            imports.append(self.parse_import())
        decls = []
        while not self.at(Kind.EOF):
            decls.append(self.parse_decl())
        return ast.File(imports, decls)

    def parse_import(self) -> ast.Import:
        start = self.peek().offset
        self.expect(Kind.IMPORT)
        path = self.expect(Kind.STRING).string_value()
        alias = self.expect(Kind.NAME).text if self.eat(Kind.AS) else None
        glob = False
        uses = []
        if self.eat(Kind.USE):
            if self.eat(Kind.STAR):
                glob = True
            else:
                uses = self.comma_list(self.parse_use_item, Kind.SEMI)
        self.expect(Kind.SEMI)
        return ast.Import(path, alias, glob, uses, offset=start)

    def parse_use_name(self) -> ast.UseName:
        dotted = bool(self.eat(Kind.DOT))
        return ast.UseName(dotted, self.expect(Kind.NAME).text)

    def parse_use_item(self) -> ast.UseItem:
        start = self.peek().offset
        name = self.parse_use_name()
        alias = self.parse_use_name() if self.eat(Kind.AS) else None
        if alias is not None and alias.dotted != name.dotted:
            raise ParseError("a use rename must keep the name's dottedness", self.peek())
        return ast.UseItem(name, alias, offset=start)

    def parse_decl(self) -> ast.Decl:
        start = self.peek().offset
        if self.at(Kind.ASSUME):
            return self.parse_assume()
        if self.at(Kind.TYPE):
            return self.parse_typedecl()
        if self.at(Kind.UNIT):
            self.next()
            name = self.expect(Kind.NAME).text
            self.expect(Kind.SEMI)
            return ast.Unitdef(name, offset=start)
        if self.at(Kind.VAL):
            self.next()
            name = self.expect(Kind.NAME).text
            self.expect(Kind.COLON)
            ty = self.parse_type()
            init = self.parse_expr() if self.eat(Kind.EQUAL) else None
            self.expect(Kind.SEMI)
            return ast.Valdef(name, ty, init, offset=start)
        if self.at(Kind.FN):
            return self.parse_fndef()
        if self.at(Kind.CONTEXT):
            self.next()
            name = self.expect(Kind.NAME).text
            self.expect(Kind.EQUAL)
            items = self.comma_list(self.parse_spec, Kind.SEMI)
            self.expect(Kind.SEMI)
            return ast.Ctxdef(name, items, offset=start)
        if self.at(Kind.FUNCTOR):
            return self.parse_functordef()
        raise ParseError(f"expected a declaration, found {self.peek().kind.name}", self.peek())

    def parse_functordef(self) -> ast.Functordef:
        start = self.peek().offset
        self.expect(Kind.FUNCTOR)
        name = self.expect(Kind.NAME).text
        self.expect(Kind.COLON)
        args = self.comma_list(self.parse_spec, Kind.HYPHEN_GREATER)
        self.expect(Kind.HYPHEN_GREATER)
        result = self.comma_list(self.parse_spec, Kind.LBRACE)
        self.expect(Kind.LBRACE)
        binds = []
        while not self.at(Kind.RBRACE):
            binds.append(self.parse_bind())
        self.expect(Kind.RBRACE)
        return ast.Functordef(name, args, result, binds, offset=start)

    def parse_bind(self) -> ast.Bind:
        start = self.peek().offset
        self.expect(Kind.BIND)

        def bind_item() -> tuple[ast.Spec, "ast.Expr | None"]:
            spec = self.parse_spec()
            if not self.eat(Kind.EQUAL):
                return spec, None  # a functor application (D55)
            return spec, self.parse_expr()

        items = self.comma_list(bind_item, Kind.SEMI)
        self.expect(Kind.SEMI)
        return ast.Bind(items, offset=start)

    def parse_assume(self) -> ast.Assume:
        start = self.peek().offset
        self.expect(Kind.ASSUME)
        items = self.comma_list(self.parse_assume_item, Kind.LBRACE)
        self.expect(Kind.LBRACE)
        decls = []
        while not self.at(Kind.RBRACE):
            decls.append(self.parse_decl())
        self.expect(Kind.RBRACE)
        return ast.Assume(items, decls, offset=start)

    def parse_typedecl(self) -> ast.Decl:
        start = self.peek().offset
        self.expect(Kind.TYPE)
        name = self.expect(Kind.NAME).text
        if self.eat(Kind.SEMI):
            return ast.Tydef(name, offset=start)
        if self.eat(Kind.EQUAL):
            ty = self.parse_type()
            self.expect(Kind.SEMI)
            return ast.Aliasdef(name, ty, offset=start)
        ty = self.parse_type()
        self.expect(Kind.SEMI)
        return ast.Tagdef(name, ty, offset=start)

    def parse_fndef(self) -> ast.Fndef:
        start = self.peek().offset
        self.expect(Kind.FN)
        receiver = None
        dotted = False
        if self.eat(Kind.DOT):
            dotted = True
            name = self.expect(Kind.NAME).text
        else:
            name = self.expect(Kind.NAME).text
            if self.eat(Kind.DOT):
                receiver = name
                name = self.expect(Kind.NAME).text
        self.expect(Kind.LPAREN)

        def param() -> ast.Param:
            pname = self.expect(Kind.NAME).text
            self.expect(Kind.COLON)
            return ast.Param(pname, self.parse_type())

        params = self.comma_list(param, Kind.RPAREN)
        self.expect(Kind.RPAREN)
        ret = self.parse_type() if self.eat(Kind.COLON) else None
        if self.eat(Kind.SEMI):
            body = None
        else:
            body = self.parse_block()
        return ast.Fndef(ast.FnName(receiver, dotted, name), params, ret, body, offset=start)

    # Statements and blocks

    def parse_block(self) -> ast.Block:
        block_start = self.peek().offset
        self.expect(Kind.LBRACE)
        stmts = []
        tail = None
        while not self.at(Kind.RBRACE):
            start = self.peek().offset
            if self.at(Kind.LET) or self.at(Kind.VAR):
                keyword = self.next()
                name = self.expect(Kind.NAME).text
                ty = self.parse_type() if self.eat(Kind.COLON) else None
                self.expect(Kind.EQUAL)
                expr = self.parse_expr()
                self.expect(Kind.SEMI)
                node = ast.Let if keyword.kind == Kind.LET else ast.Var
                stmts.append(node(name, ty, expr, offset=start))
            elif self.at(Kind.BIND):
                stmts.append(self.parse_bind())
            elif self.at(Kind.WHILE):
                self.next()
                cond = self.parse_expr(no_record=True)
                stmts.append(ast.While(cond, self.parse_block(), offset=start))
            elif self.at(Kind.LOOP):
                self.next()
                stmts.append(ast.Loop(self.parse_block(), offset=start))
            elif self.at(Kind.NAME) and self.at(Kind.EQUAL, 1):
                name = self.next().text
                self.next()
                expr = self.parse_expr()
                self.expect(Kind.SEMI)
                stmts.append(ast.Assign(name, expr, offset=start))
            else:
                # Expressions ending in `}` may stand without a `;`: `if`,
                # `match`, and a bare block (D65).
                braced = self.at_any(Kind.IF, Kind.MATCH, Kind.LBRACE)
                expr = self.parse_expr()
                if self.eat(Kind.SEMI):
                    stmts.append(ast.ExprStmt(expr, offset=start))
                elif self.at(Kind.RBRACE):
                    tail = expr
                elif braced:
                    stmts.append(ast.ExprStmt(expr, offset=start))
                else:
                    raise ParseError(
                        f"expected SEMI or RBRACE, found {self.peek().kind.name}", self.peek()
                    )
        self.expect(Kind.RBRACE)
        return ast.Block(stmts, tail, offset=block_start)

    # Expressions

    def parse_expr(self, no_record: bool = False) -> ast.Expr:
        start = self.peek().offset
        if self.at(Kind.IF):
            return self.parse_if()
        if self.at(Kind.MATCH):
            self.next()
            scrutinee = self.parse_expr(no_record=True)
            self.expect(Kind.LBRACE)
            arms = []
            while not self.at(Kind.RBRACE):
                arms.append(self.parse_arm())
            self.expect(Kind.RBRACE)
            return ast.Match(scrutinee, arms, offset=start)
        if self.eat(Kind.RETURN):
            expr = None
            if not self.at_any(Kind.SEMI, Kind.COMMA, Kind.RBRACE, Kind.RPAREN):
                expr = self.parse_expr(no_record)
            return ast.Return(expr, offset=start)
        if self.eat(Kind.BREAK):
            return ast.Break(offset=start)
        return self.parse_operators(no_record)

    # Operators (D68). Precedence is a partial order: where habits
    # differ, two operators are simply not ordered, and meeting without
    # parentheses is an error rather than a guess.
    #
    #   comparison:  operand (cmp operand)?          -- does not chain
    #   operand:     unary (<< | >>) unary           -- shifts take unaries
    #              | additive
    #   additive:    term ((+ | -) term)*
    #   term:        unary (* unary)* ((/ | %) unary)?
    #   bitwise:     unary (& unary)*, likewise ^ and |, each alone

    def binop(self) -> str | None:
        return BINARY.get(self.peek().kind)

    def mixed(self, before: str, token: Token):
        after = BINARY[token.kind]
        if before in COMPARISON and after in COMPARISON:
            message = "comparisons do not chain"
        elif before in SHIFT and after in SHIFT:
            message = "shifts do not chain"
        elif before == after:
            message = f"`{after}` does not chain here"
        else:
            message = f"`{before}` and `{after}` are not ordered"
        raise ParseError(f"{message}; add parentheses (D68)", token)

    def binary(self, op: str, lhs: ast.Expr, rhs: ast.Expr) -> ast.Binary:
        return ast.Binary(op, lhs, rhs, offset=lhs.offset)

    def parse_operators(self, no_record: bool) -> ast.Expr:
        first = self.parse_unary(no_record)
        op = self.binop()
        if op is None:
            return first
        if op in BITWISE:
            expr = first
            while self.binop() == op:
                self.next()
                expr = self.binary(op, expr, self.parse_unary(no_record))
            if self.binop() is not None:
                self.mixed(op, self.peek())
            return expr
        lhs, last = self.parse_operand(first, no_record)
        if self.binop() not in COMPARISON:
            if self.binop() is not None:
                self.mixed(last, self.peek())
            return lhs
        op = BINARY[self.next().kind]
        rhs, last = self.parse_operand(self.parse_unary(no_record), no_record)
        if self.binop() is not None:
            self.mixed(op if last is None else last, self.peek())
        return self.binary(op, lhs, rhs)

    def parse_operand(self, first: ast.Expr, no_record: bool):
        """A comparison's operand: a shift, or an arithmetic expression.
        Returns it and the last operator it ended on, for the message if
        an operator that may not follow does."""
        op = self.binop()
        if op in SHIFT:
            self.next()
            return self.binary(op, first, self.parse_unary(no_record)), op
        expr, last = self.parse_term(first, no_record)
        while self.binop() in ADDITIVE:
            op = BINARY[self.next().kind]
            rhs, _ = self.parse_term(self.parse_unary(no_record), no_record)
            expr = self.binary(op, expr, rhs)
            last = op
        if last is not None and self.binop() in SHIFT:
            self.mixed(last, self.peek())
        return expr, last

    def parse_term(self, first: ast.Expr, no_record: bool):
        expr = first
        last = None
        while self.binop() == "*":
            self.next()
            expr = self.binary("*", expr, self.parse_unary(no_record))
            last = "*"
        if self.binop() in FRACTION:
            op = BINARY[self.next().kind]
            expr = self.binary(op, expr, self.parse_unary(no_record))
            last = op
            if self.binop() in ("*", *FRACTION):
                self.mixed(op, self.peek())
        return expr, last

    def parse_unary(self, no_record: bool) -> ast.Expr:
        start = self.peek().offset
        op = UNARY.get(self.peek().kind)
        if op is None:
            return self.parse_postfix(no_record)
        self.next()
        if (
            op == "-"
            and self.at_any(Kind.INTEGER, Kind.DECIMAL)
            and self.at(Kind.DOT, 1)
        ):
            # `-1.abs()` would be `-(1.abs())`, which nobody means.
            text = self.peek().text
            name = self.peek(2).text if self.at(Kind.NAME, 2) else "m"
            raise ParseError(
                f"`-` before a literal with a method call: write `(-{text}).{name}(...)` "
                f"or `-({text}.{name}(...))` (D68)",
                self.peek(),
            )
        return ast.Unary(op, self.parse_unary(no_record), offset=start)

    def parse_if(self) -> ast.If:
        start = self.peek().offset
        self.expect(Kind.IF)
        cond = self.parse_expr(no_record=True)
        then = self.parse_block()
        els = None
        if self.eat(Kind.ELSE):
            els = self.parse_if() if self.at(Kind.IF) else self.parse_block()
        return ast.If(cond, then, els, offset=start)

    def parse_arm(self) -> ast.Arm:
        start = self.peek().offset
        pattern = self.parse_pattern()
        self.expect(Kind.EQUAL_GREATER)
        if self.at(Kind.LBRACE):
            body = self.parse_block()
            self.eat(Kind.COMMA)
            return ast.Arm(pattern, body, offset=start)
        body = self.parse_expr()
        if not self.at(Kind.RBRACE):
            self.expect(Kind.COMMA)
        return ast.Arm(pattern, body, offset=start)

    def parse_postfix(self, no_record: bool) -> ast.Expr:
        start = self.peek().offset
        expr = self.parse_primary(no_record)
        while self.at(Kind.DOT):
            self.next()
            path = self.parse_path()
            if self.at(Kind.LPAREN):
                self.next()
                args = self.comma_list(self.parse_expr, Kind.RPAREN)
                self.expect(Kind.RPAREN)
                expr = ast.MethodCall(expr, path, args, offset=start)
            else:
                if len(path) != 1:
                    raise ParseError("field access takes a single name", self.peek())
                expr = ast.Field(expr, path[0], offset=start)
        return expr

    def parse_primary(self, no_record: bool) -> ast.Expr:
        start = self.peek().offset
        if self.eat(Kind.LPAREN):
            if self.eat(Kind.RPAREN):
                return ast.UnitExpr(offset=start)
            expr = self.parse_expr()
            self.expect(Kind.RPAREN)
            return expr
        if self.eat(Kind.THIS):
            return ast.ThisExpr(offset=start)
        if self.at_any(Kind.INTEGER, Kind.DECIMAL):
            return ast.Literal(self.next().text, offset=start)
        if self.at(Kind.LBRACE) and not no_record:
            # A bare block (D65). Where a record expression is forbidden
            # (an `if`/`while` condition, a `match` scrutinee), so is this:
            # the brace there opens the body.
            return self.parse_block()
        if self.at(Kind.NAME):
            path = self.parse_path()
            app = self.parse_app() if self.at(Kind.LBRACKET) else None
            callee = ast.PathExpr(path, app, offset=start)
            if self.at(Kind.LPAREN):
                self.next()
                args = self.comma_list(self.parse_expr, Kind.RPAREN)
                self.expect(Kind.RPAREN)
                return ast.Call(callee, args, offset=start)
            if self.at(Kind.LBRACE) and not no_record:
                self.next()

                def field() -> tuple[str, ast.Expr | None]:
                    name = self.expect(Kind.NAME).text
                    value = self.parse_expr() if self.eat(Kind.EQUAL) else None
                    return name, value

                fields = self.comma_list(field, Kind.RBRACE)
                self.expect(Kind.RBRACE)
                return ast.RecordExpr(callee, fields, offset=start)
            return callee
        raise ParseError(f"expected an expression, found {self.peek().kind.name}", self.peek())

    # Patterns

    def parse_pattern(self) -> ast.Pattern:
        start = self.peek().offset
        if self.at(Kind.LBRACE):
            return self.parse_record_pattern(None)
        if self.at(Kind.NAME):
            if self.peek().text == "_" and not self.at(Kind.COLON_COLON, 1):
                self.next()
                return ast.PatWild(offset=start)
            path = self.parse_path()
            if self.at(Kind.LBRACE):
                return self.parse_record_pattern(path)
            if self.at(Kind.NAME):
                return ast.PatTag(path, self.parse_pattern(), offset=start)
            return ast.PatPath(path, offset=start)
        raise ParseError(f"expected a pattern, found {self.peek().kind.name}", self.peek())

    def parse_record_pattern(self, path: ast.Path | None) -> ast.PatRecord:
        start = self.peek().offset
        self.expect(Kind.LBRACE)

        def field() -> tuple[str, ast.Pattern | None]:
            name = self.expect(Kind.NAME).text
            value = self.parse_pattern() if self.eat(Kind.EQUAL) else None
            return name, value

        fields = self.comma_list(field, Kind.RBRACE)
        self.expect(Kind.RBRACE)
        return ast.PatRecord(path, fields, offset=start)


def parse(source: str) -> ast.File:
    return Parser(lex(source)).parse_file()


def error_message(source: str, error: ParseError) -> str:
    line, col = position(source, error.token.offset)
    return f"{line}:{col}: {error.message}"
