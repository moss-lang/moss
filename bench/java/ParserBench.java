import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

public class ParserBench {
    enum Kind {
        EOF,
        EXCLAM,
        PERCENT,
        LPAREN,
        RPAREN,
        STAR,
        PLUS,
        COMMA,
        HYPHEN,
        DOT,
        SLASH,
        COLON,
        SEMI,
        LESS,
        EQUAL,
        GREATER,
        LBRACKET,
        RBRACKET,
        LBRACE,
        RBRACE,
        EXCLAM_EQUAL,
        COLONCOLON,
        LESS_EQUAL,
        EQUAL_EQUAL,
        GREATER_EQUAL,
        AS,
        CONTEXT,
        ELSE,
        FN,
        IF,
        IMPORT,
        LET,
        STATIC,
        STRUCT,
        THIS,
        TYPE,
        USE,
        VAL,
        VAR,
        WHILE,
        NAME,
        STR,
        INT
    }

    static final class Tok {
        final Kind kind;
        final String text;

        Tok(Kind kind, String text) {
            this.kind = kind;
            this.text = text;
        }
    }

    static final class Lexer {
        private final String src;
        private final int len;
        private int i = 0;

        Lexer(String src) {
            this.src = src;
            this.len = src.length();
        }

        List<Tok> lex() {
            ArrayList<Tok> tokens = new ArrayList<>();
            while (true) {
                skipWsAndComments();
                if (i >= len) {
                    tokens.add(new Tok(Kind.EOF, ""));
                    return tokens;
                }
                char c = src.charAt(i);
                switch (c) {
                    case '!':
                        if (peek2("!=")) {
                            tokens.add(new Tok(Kind.EXCLAM_EQUAL, "!="));
                            i += 2;
                        } else {
                            tokens.add(new Tok(Kind.EXCLAM, "!"));
                            i++;
                        }
                        break;
                    case '%':
                        tokens.add(new Tok(Kind.PERCENT, "%"));
                        i++;
                        break;
                    case '(': tokens.add(new Tok(Kind.LPAREN, "(")); i++; break;
                    case ')': tokens.add(new Tok(Kind.RPAREN, ")")); i++; break;
                    case '*': tokens.add(new Tok(Kind.STAR, "*")); i++; break;
                    case '+': tokens.add(new Tok(Kind.PLUS, "+")); i++; break;
                    case ',': tokens.add(new Tok(Kind.COMMA, ",")); i++; break;
                    case '-': tokens.add(new Tok(Kind.HYPHEN, "-")); i++; break;
                    case '.': tokens.add(new Tok(Kind.DOT, ".")); i++; break;
                    case '/': tokens.add(new Tok(Kind.SLASH, "/")); i++; break;
                    case ':':
                        if (peek2("::")) {
                            tokens.add(new Tok(Kind.COLONCOLON, "::"));
                            i += 2;
                        } else {
                            tokens.add(new Tok(Kind.COLON, ":"));
                            i++;
                        }
                        break;
                    case ';': tokens.add(new Tok(Kind.SEMI, ";")); i++; break;
                    case '<':
                        if (peek2("<=")) {
                            tokens.add(new Tok(Kind.LESS_EQUAL, "<="));
                            i += 2;
                        } else {
                            tokens.add(new Tok(Kind.LESS, "<"));
                            i++;
                        }
                        break;
                    case '=':
                        if (peek2("==")) {
                            tokens.add(new Tok(Kind.EQUAL_EQUAL, "=="));
                            i += 2;
                        } else {
                            tokens.add(new Tok(Kind.EQUAL, "="));
                            i++;
                        }
                        break;
                    case '>':
                        if (peek2(">=")) {
                            tokens.add(new Tok(Kind.GREATER_EQUAL, ">="));
                            i += 2;
                        } else {
                            tokens.add(new Tok(Kind.GREATER, ">"));
                            i++;
                        }
                        break;
                    case '[': tokens.add(new Tok(Kind.LBRACKET, "[")); i++; break;
                    case ']': tokens.add(new Tok(Kind.RBRACKET, "]")); i++; break;
                    case '{': tokens.add(new Tok(Kind.LBRACE, "{")); i++; break;
                    case '}': tokens.add(new Tok(Kind.RBRACE, "}")); i++; break;
                    case '"':
                        tokens.add(string());
                        break;
                    default:
                        if (isAlpha(c) || c == '_') {
                            tokens.add(name());
                        } else if (isDigit(c)) {
                            tokens.add(intToken());
                        } else {
                            throw new RuntimeException("unexpected char: " + c);
                        }
                }
            }
        }

        private Tok string() {
            int start = i;
            i++; // opening quote
            while (i < len) {
                char c = src.charAt(i);
                if (c == '\\') {
                    i += 2;
                    continue;
                }
                if (c == '"') {
                    i++;
                    break;
                }
                i++;
            }
            return new Tok(Kind.STR, src.substring(start, i));
        }

        private Tok name() {
            int start = i;
            i++;
            while (i < len && (isAlpha(src.charAt(i)) || isDigit(src.charAt(i)) || src.charAt(i) == '_')) {
                i++;
            }
            String text = src.substring(start, i);
            Kind kw = keyword(text);
            return new Tok(kw, text);
        }

        private Tok intToken() {
            int start = i;
            while (i < len && isDigit(src.charAt(i))) {
                i++;
            }
            return new Tok(Kind.INT, src.substring(start, i));
        }

        private Kind keyword(String text) {
            switch (text) {
                case "as": return Kind.AS;
                case "context": return Kind.CONTEXT;
                case "else": return Kind.ELSE;
                case "fn": return Kind.FN;
                case "if": return Kind.IF;
                case "import": return Kind.IMPORT;
                case "let": return Kind.LET;
                case "static": return Kind.STATIC;
                case "struct": return Kind.STRUCT;
                case "this": return Kind.THIS;
                case "type": return Kind.TYPE;
                case "use": return Kind.USE;
                case "val": return Kind.VAL;
                case "var": return Kind.VAR;
                case "while": return Kind.WHILE;
                default: return Kind.NAME;
            }
        }

        private boolean peek2(String s) {
            return src.startsWith(s, i);
        }

        private void skipWsAndComments() {
            while (i < len) {
                char c = src.charAt(i);
                if (Character.isWhitespace(c)) {
                    i++;
                    continue;
                }
                if (c == '#') {
                    while (i < len && src.charAt(i) != '\n') i++;
                    continue;
                }
                break;
            }
        }

        private boolean isAlpha(char c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
        }

        private boolean isDigit(char c) {
            return c >= '0' && c <= '9';
        }
    }

    static final class ParseError extends RuntimeException {
        ParseError(String msg) { super(msg); }
    }

    static final class PathNode {
        final List<Tok> prefix;
        final Tok last;
        PathNode(List<Tok> prefix, Tok last) { this.prefix = prefix; this.last = last; }
    }

    static final class Need {
        final boolean isStatic;
        final PathNode path;
        Need(boolean isStatic, PathNode path) { this.isStatic = isStatic; this.path = path; }
    }

    static final class Bind { final PathNode path; final Expr val; Bind(PathNode path, Expr val){this.path=path;this.val=val;} }
    static final class Param { final Tok name; final PathNode ty; Param(Tok name, PathNode ty){this.name=name;this.ty=ty;} }
    static final class Field { final Tok name; final Expr val; Field(Tok name, Expr val){this.name=name;this.val=val;} }

    static abstract class Expr {}
    static final class ExprThis extends Expr { final Tok tok; ExprThis(Tok tok){this.tok=tok;} }
    static final class ExprPath extends Expr { final PathNode path; ExprPath(PathNode path){this.path=path;} }
    static final class ExprInt extends Expr { final Tok tok; ExprInt(Tok tok){this.tok=tok;} }
    static final class ExprString extends Expr { final Tok tok; ExprString(Tok tok){this.tok=tok;} }
    static final class ExprStruct extends Expr { final PathNode path; final List<Field> fields; ExprStruct(PathNode path,List<Field> fields){this.path=path;this.fields=fields;} }
    static final class ExprField extends Expr { final Expr object; final Tok name; ExprField(Expr object, Tok name){this.object=object;this.name=name;} }
    static final class ExprMethod extends Expr { final Expr object; final Tok name; final List<Expr> args; ExprMethod(Expr object, Tok name, List<Expr> args){this.object=object;this.name=name;this.args=args;} }
    static final class ExprCall extends Expr { final PathNode path; final List<Bind> binds; final List<Expr> args; ExprCall(PathNode path,List<Bind> binds,List<Expr> args){this.path=path;this.binds=binds;this.args=args;} }
    static final class ExprBinary extends Expr { final Expr lhs; final Kind op; final Expr rhs; ExprBinary(Expr lhs, Kind op, Expr rhs){this.lhs=lhs;this.op=op;this.rhs=rhs;} }
    static final class ExprIf extends Expr { final Expr cond; final Block thenBlk; final Block elseBlk; ExprIf(Expr cond, Block thenBlk, Block elseBlk){this.cond=cond;this.thenBlk=thenBlk;this.elseBlk=elseBlk;} }

    static abstract class Stmt {}
    static final class StmtLet extends Stmt { final Tok name; final Expr expr; StmtLet(Tok name, Expr expr){this.name=name;this.expr=expr;} }
    static final class StmtVar extends Stmt { final Tok name; final Expr expr; StmtVar(Tok name, Expr expr){this.name=name;this.expr=expr;} }
    static final class StmtAssign extends Stmt { final Expr lhs; final Expr rhs; StmtAssign(Expr lhs, Expr rhs){this.lhs=lhs;this.rhs=rhs;} }
    static final class StmtWhile extends Stmt { final Expr cond; final Block body; StmtWhile(Expr cond, Block body){this.cond=cond;this.body=body;} }
    static final class StmtExpr extends Stmt { final Expr expr; StmtExpr(Expr expr){this.expr=expr;} }

    static final class Block {
        final List<Stmt> stmts;
        final Expr expr; // optional tail expression
        Block(List<Stmt> stmts, Expr expr){this.stmts=stmts;this.expr=expr;}
    }

    static final class Import { final Tok from; final Tok name; final List<Tok> names; final List<Method> methods; Import(Tok from, Tok name, List<Tok> names, List<Method> methods){this.from=from;this.name=name;this.names=names;this.methods=methods;} }
    static final class Method { final Tok ty; final Tok name; Method(Tok ty, Tok name){this.ty=ty;this.name=name;} }
    static final class Tydef { final Tok name; final PathNode def; Tydef(Tok name, PathNode def){this.name=name;this.def=def;} }
    static final class Fndef { final Tok ty; final Tok name; final List<Need> needs; final List<Param> params; final PathNode result; final Block def; Fndef(Tok ty, Tok name, List<Need> needs, List<Param> params, PathNode result, Block def){this.ty=ty;this.name=name;this.needs=needs;this.params=params;this.result=result;this.def=def;} }
    static final class Valdef { final Tok name; final List<Need> needs; final PathNode ty; Valdef(Tok name, List<Need> needs, PathNode ty){this.name=name;this.needs=needs;this.ty=ty;} }
    static final class Ctxdef { final Tok name; final List<Need> def; Ctxdef(Tok name,List<Need> def){this.name=name;this.def=def;} }
    static final class Structdef { final Tok name; final List<Param> fields; Structdef(Tok name, List<Param> fields){this.name=name;this.fields=fields;} }

    static final class Parser {
        private final List<Tok> tokens;
        private int idx = 0;

        Parser(List<Tok> tokens) { this.tokens = tokens; }

        List<Object> parse() {
            ArrayList<Object> items = new ArrayList<>();
            while (true) {
                switch (peek().kind) {
                    case IMPORT: items.add(importDecl()); break;
                    case TYPE: items.add(tydef()); break;
                    case FN: items.add(fndef()); break;
                    case VAL: items.add(valdef()); break;
                    case CONTEXT: items.add(ctxdef()); break;
                    case STRUCT: items.add(structdef()); break;
                    case EOF: return items;
                    default: throw err(EnumSet.of(Kind.IMPORT, Kind.TYPE, Kind.FN, Kind.VAL, Kind.CONTEXT, Kind.STRUCT, Kind.EOF));
                }
            }
        }

        private Tok peek() { return tokens.get(idx); }
        private Tok next() { Tok t = peek(); if (t.kind == Kind.EOF) throw new ParseError("unexpected eof"); idx++; return t; }
        private ParseError err(EnumSet<Kind> expected) { return new ParseError("expected " + expected); }
        private Tok expect(Kind k) { Tok t = peek(); if (t.kind != k) throw err(EnumSet.of(k)); idx++; return t; }

        private PathNode path() {
            ArrayList<Tok> prefix = new ArrayList<>();
            Tok last = expect(Kind.NAME);
            while (peek().kind == Kind.COLONCOLON) {
                next();
                prefix.add(last);
                last = expect(Kind.NAME);
            }
            return new PathNode(prefix, last);
        }

        private Need need() {
            boolean isStatic = false;
            if (peek().kind == Kind.STATIC) { next(); isStatic = true; }
            PathNode path = path();
            return new Need(isStatic, path);
        }

        private List<Need> needs() {
            expect(Kind.LBRACKET);
            ArrayList<Need> needs = new ArrayList<>();
            while (true) {
                if (peek().kind == Kind.RBRACKET) { next(); return needs; }
                needs.add(need());
                if (peek().kind == Kind.COMMA) next();
            }
        }

        private Bind bind() {
            PathNode path = path();
            expect(Kind.EQUAL);
            Expr val = expr(Curly.Yes);
            return new Bind(path, val);
        }

        private Param param() {
            Tok name = expect(Kind.NAME);
            expect(Kind.COLON);
            PathNode ty = path();
            return new Param(name, ty);
        }

        private Field field() {
            Tok name = expect(Kind.NAME);
            Expr val;
            if (peek().kind == Kind.EQUAL) {
                next();
                val = expr(Curly.Yes);
            } else {
                val = new ExprPath(new PathNode(new ArrayList<>(), name));
            }
            return new Field(name, val);
        }

        private List<Expr> args() {
            expect(Kind.LPAREN);
            ArrayList<Expr> args = new ArrayList<>();
            while (true) {
                if (peek().kind == Kind.RPAREN) { next(); return args; }
                args.add(expr(Curly.Yes));
                if (peek().kind == Kind.COMMA) next();
            }
        }

        private Expr exprIf() {
            expect(Kind.IF);
            Expr cond = expr(Curly.No);
            Block thenBlk = block();
            Block elseBlk = null;
            if (peek().kind == Kind.ELSE) {
                next();
                elseBlk = block();
            }
            return new ExprIf(cond, thenBlk, elseBlk);
        }

        private Expr exprAtom(Curly curly) {
            switch (peek().kind) {
                case LPAREN: {
                    next();
                    Expr expr = expr(curly);
                    expect(Kind.RPAREN);
                    return expr;
                }
                case IF: return exprIf();
                case THIS: return new ExprThis(next());
                case INT: return new ExprInt(next());
                case STR: return new ExprString(next());
                case NAME: {
                    PathNode path = path();
                    switch (peek().kind) {
                        case LPAREN: {
                            List<Expr> args = args();
                            return new ExprCall(path, new ArrayList<>(), args);
                        }
                        case LBRACKET: {
                            next();
                            ArrayList<Bind> binds = new ArrayList<>();
                            while (true) {
                                if (peek().kind == Kind.RBRACKET) { next(); break; }
                                binds.add(bind());
                                if (peek().kind == Kind.COMMA) next();
                            }
                            List<Expr> args = args();
                            return new ExprCall(path, binds, args);
                        }
                        case LBRACE: {
                            if (curly == Curly.No) return new ExprPath(path);
                            next();
                            ArrayList<Field> fields = new ArrayList<>();
                            while (true) {
                                if (peek().kind == Kind.RBRACE) { next(); return new ExprStruct(path, fields); }
                                fields.add(field());
                                if (peek().kind == Kind.COMMA) next();
                            }
                        }
                        default:
                            return new ExprPath(path);
                    }
                }
                default:
                    throw err(EnumSet.of(Kind.LPAREN, Kind.IF, Kind.THIS, Kind.INT, Kind.STR, Kind.NAME));
            }
        }

        private Expr exprChain(Curly curly) {
            Expr expr = exprAtom(curly);
            while (peek().kind == Kind.DOT) {
                next();
                Tok name = expect(Kind.NAME);
                if (peek().kind == Kind.LPAREN) {
                    List<Expr> args = args();
                    expr = new ExprMethod(expr, name, args);
                } else {
                    expr = new ExprField(expr, name);
                }
            }
            return expr;
        }

        private Expr exprFactor(Curly curly) { return exprChain(curly); }

        private Expr exprTerm(Curly curly) {
            Expr lhs = exprFactor(curly);
            while (true) {
                Kind op;
                switch (peek().kind) {
                    case PERCENT: op = Kind.PERCENT; break;
                    case STAR: op = Kind.STAR; break;
                    case SLASH: op = Kind.SLASH; break;
                    default: return lhs;
                }
                next();
                Expr rhs = exprFactor(curly);
                lhs = new ExprBinary(lhs, op, rhs);
            }
        }

        private Expr exprQuant(Curly curly) {
            Expr lhs = exprTerm(curly);
            while (true) {
                Kind op;
                switch (peek().kind) {
                    case PLUS: op = Kind.PLUS; break;
                    case HYPHEN: op = Kind.HYPHEN; break;
                    default: return lhs;
                }
                next();
                Expr rhs = exprTerm(curly);
                lhs = new ExprBinary(lhs, op, rhs);
            }
        }

        private Expr exprComp(Curly curly) {
            Expr lhs = exprQuant(curly);
            Kind op;
            switch (peek().kind) {
                case LESS: op = Kind.LESS; break;
                case GREATER: op = Kind.GREATER; break;
                case EXCLAM_EQUAL: op = Kind.EXCLAM_EQUAL; break;
                case LESS_EQUAL: op = Kind.LESS_EQUAL; break;
                case EQUAL_EQUAL: op = Kind.EQUAL_EQUAL; break;
                case GREATER_EQUAL: op = Kind.GREATER_EQUAL; break;
                default: return lhs;
            }
            next();
            Expr rhs = exprQuant(curly);
            return new ExprBinary(lhs, op, rhs);
        }

        private Expr expr(Curly curly) { return exprComp(curly); }

        private Block block() {
            expect(Kind.LBRACE);
            ArrayList<Stmt> stmts = new ArrayList<>();
            while (true) {
                switch (peek().kind) {
                    case RBRACE:
                        next();
                        return new Block(stmts, null);
                    case LET: {
                        next();
                        Tok name = expect(Kind.NAME);
                        expect(Kind.EQUAL);
                        Expr expr = expr(Curly.Yes);
                        expect(Kind.SEMI);
                        stmts.add(new StmtLet(name, expr));
                        break;
                    }
                    case VAR: {
                        next();
                        Tok name = expect(Kind.NAME);
                        expect(Kind.EQUAL);
                        Expr expr = expr(Curly.Yes);
                        expect(Kind.SEMI);
                        stmts.add(new StmtVar(name, expr));
                        break;
                    }
                    case WHILE: {
                        next();
                        Expr cond = expr(Curly.No);
                        Block body = block();
                        stmts.add(new StmtWhile(cond, body));
                        break;
                    }
                    default: {
                        Expr expr = expr(Curly.Yes);
                        if (expr instanceof ExprIf) {
                            stmts.add(new StmtExpr(expr));
                            continue;
                        }
                        switch (peek().kind) {
                            case EQUAL: {
                                next();
                                Expr rhs = expr(Curly.Yes);
                                expect(Kind.SEMI);
                                stmts.add(new StmtAssign(expr, rhs));
                                break;
                            }
                            case SEMI: {
                                next();
                                stmts.add(new StmtExpr(expr));
                                break;
                            }
                            case RBRACE: {
                                next();
                                return new Block(stmts, expr);
                            }
                            default:
                                throw err(EnumSet.of(Kind.EQUAL, Kind.SEMI, Kind.RBRACE));
                        }
                    }
                }
            }
        }

        private Import importDecl() {
            expect(Kind.IMPORT);
            Tok from = expect(Kind.STR);
            Tok name = null;
            if (peek().kind == Kind.AS) {
                next();
                name = expect(Kind.NAME);
            }
            ArrayList<Tok> names = new ArrayList<>();
            ArrayList<Method> methods = new ArrayList<>();
            if (peek().kind == Kind.USE) {
                next();
                while (true) {
                    if (peek().kind == Kind.SEMI) break;
                    Tok item = expect(Kind.NAME);
                    if (peek().kind == Kind.DOT) {
                        next();
                        Tok method = expect(Kind.NAME);
                        methods.add(new Method(item, method));
                    } else {
                        names.add(item);
                    }
                    if (peek().kind == Kind.COMMA) next();
                }
            }
            expect(Kind.SEMI);
            return new Import(from, name, names, methods);
        }

        private Tydef tydef() {
            expect(Kind.TYPE);
            Tok name = expect(Kind.NAME);
            PathNode def = null;
            if (peek().kind == Kind.EQUAL) {
                next();
                def = path();
            }
            expect(Kind.SEMI);
            return new Tydef(name, def);
        }

        private Fndef fndef() {
            expect(Kind.FN);
            Tok name = expect(Kind.NAME);
            Tok ty = null;
            if (peek().kind == Kind.DOT) {
                next();
                ty = name;
                name = expect(Kind.NAME);
            }
            List<Need> needs = peek().kind == Kind.LBRACKET ? needs() : new ArrayList<>();
            expect(Kind.LPAREN);
            ArrayList<Param> params = new ArrayList<>();
            while (true) {
                if (peek().kind == Kind.RPAREN) { next(); break; }
                params.add(param());
                if (peek().kind == Kind.COMMA) next();
            }
            PathNode result = null;
            if (peek().kind == Kind.COLON) {
                next();
                result = path();
            }
            Block def;
            switch (peek().kind) {
                case SEMI: next(); def = null; break;
                case LBRACE: def = block(); break;
                default: throw err(EnumSet.of(Kind.SEMI, Kind.LBRACE));
            }
            return new Fndef(ty, name, needs, params, result, def);
        }

        private Valdef valdef() {
            expect(Kind.VAL);
            Tok name = expect(Kind.NAME);
            List<Need> needs = peek().kind == Kind.LBRACKET ? needs() : new ArrayList<>();
            expect(Kind.COLON);
            PathNode ty = path();
            expect(Kind.SEMI);
            return new Valdef(name, needs, ty);
        }

        private Ctxdef ctxdef() {
            expect(Kind.CONTEXT);
            Tok name = expect(Kind.NAME);
            expect(Kind.EQUAL);
            List<Need> def = new ArrayList<>();
            while (true) {
                if (peek().kind == Kind.SEMI) { next(); break; }
                def.add(need());
                if (peek().kind == Kind.COMMA) next();
            }
            return new Ctxdef(name, def);
        }

        private Structdef structdef() {
            expect(Kind.STRUCT);
            Tok name = expect(Kind.NAME);
            expect(Kind.LBRACE);
            ArrayList<Param> fields = new ArrayList<>();
            while (true) {
                if (peek().kind == Kind.RBRACE) { next(); return new Structdef(name, fields); }
                fields.add(param());
                if (peek().kind == Kind.COMMA) next();
            }
        }
    }

    enum Curly { Yes, No }

    static int countPath(PathNode p) {
        return 1 + p.prefix.size();
    }

    static int countExpr(Expr e) {
        if (e == null) return 0;
        if (e instanceof ExprThis || e instanceof ExprInt || e instanceof ExprString) return 1;
        if (e instanceof ExprPath) return 1 + countPath(((ExprPath) e).path);
        if (e instanceof ExprStruct) {
            ExprStruct s = (ExprStruct) e;
            int total = 1 + countPath(s.path);
            for (Field f : s.fields) total += countField(f);
            return total;
        }
        if (e instanceof ExprField) return 1 + countExpr(((ExprField) e).object);
        if (e instanceof ExprMethod) {
            ExprMethod m = (ExprMethod) e;
            int total = 1 + countExpr(m.object);
            for (Expr arg : m.args) total += countExpr(arg);
            return total;
        }
        if (e instanceof ExprCall) {
            ExprCall c = (ExprCall) e;
            int total = 1 + countPath(c.path);
            for (Bind b : c.binds) total += countBind(b);
            for (Expr arg : c.args) total += countExpr(arg);
            return total;
        }
        if (e instanceof ExprBinary) {
            ExprBinary b = (ExprBinary) e;
            return 1 + countExpr(b.lhs) + countExpr(b.rhs);
        }
        if (e instanceof ExprIf) {
            ExprIf i = (ExprIf) e;
            return 1 + countExpr(i.cond) + countBlock(i.thenBlk) + countBlock(i.elseBlk);
        }
        throw new RuntimeException("unknown expr type" + e.getClass());
    }

    static int countStmt(Stmt s) {
        if (s instanceof StmtLet) return 1 + countExpr(((StmtLet) s).expr);
        if (s instanceof StmtVar) return 1 + countExpr(((StmtVar) s).expr);
        if (s instanceof StmtAssign) {
            StmtAssign a = (StmtAssign) s;
            return 1 + countExpr(a.lhs) + countExpr(a.rhs);
        }
        if (s instanceof StmtWhile) return 1 + countExpr(((StmtWhile) s).cond) + countBlock(((StmtWhile) s).body);
        if (s instanceof StmtExpr) return 1 + countExpr(((StmtExpr) s).expr);
        throw new RuntimeException("unknown stmt type");
    }

    static int countField(Field f) { return 1 + countExpr(f.val); }
    static int countBind(Bind b) { return 1 + countPath(b.path) + countExpr(b.val); }
    static int countNeed(Need n) { return 1 + countPath(n.path); }
    static int countParam(Param p) { return 1 + countPath(p.ty); }

    static int countBlock(Block b) {
        if (b == null) return 0;
        int total = 1;
        for (Stmt s : b.stmts) total += countStmt(s);
        total += countExpr(b.expr);
        return total;
    }

    static int countAll(List<Object> items) {
        int total = 0;
        for (Object o : items) {
            if (o instanceof Import) {
                Import im = (Import) o;
                total += 1 + im.names.size() + im.methods.size();
            } else if (o instanceof Tydef) {
                Tydef t = (Tydef) o;
                total += 1;
                if (t.def != null) total += countPath(t.def);
            } else if (o instanceof Fndef) {
                Fndef f = (Fndef) o;
                total += 1;
                for (Need n : f.needs) total += countNeed(n);
                for (Param p : f.params) total += countParam(p);
                if (f.result != null) total += countPath(f.result);
                total += countBlock(f.def);
            } else if (o instanceof Valdef) {
                Valdef v = (Valdef) o;
                total += 1;
                for (Need n : v.needs) total += countNeed(n);
                total += countPath(v.ty);
            } else if (o instanceof Ctxdef) {
                Ctxdef c = (Ctxdef) o;
                total += 1;
                for (Need n : c.def) total += countNeed(n);
            } else if (o instanceof Structdef) {
                Structdef s = (Structdef) o;
                total += 1;
                for (Param p : s.fields) total += countParam(p);
            }
        }
        return total;
    }

    static class BenchResult { final double avgMs; final int nodes; BenchResult(double avgMs, int nodes){this.avgMs=avgMs;this.nodes=nodes;} }

    static BenchResult bench(String source, int runs) {
        long total = 0;
        int lastNodes = 0;
        int checksum = 0;
        for (int i = 0; i < runs; i++) {
            long start = System.nanoTime();
            List<Tok> tokens = new Lexer(source).lex();
            List<Object> ast = new Parser(tokens).parse();
            lastNodes = countAll(ast);
            checksum += lastNodes;
            total += (System.nanoTime() - start);
        }
        // Keep checksum visible to avoid JIT dropping work.
        if (checksum == 42) {
            System.err.println("impossible");
        }
        return new BenchResult(total / (runs * 1_000_000.0), lastNodes);
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 3) {
            System.err.println("usage: ParserBench <base-path> <copies> <runs>");
            System.exit(1);
        }
        Path base = Path.of(args[0]);
        int copies = Integer.parseInt(args[1]);
        int runs = Integer.parseInt(args[2]);
        String src = Files.readString(base);
        String repeated = src.repeat(copies);
        BenchResult result = bench(repeated, runs);
        System.out.println(result.avgMs + " " + result.nodes);
    }
}
