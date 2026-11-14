// C to ARM64 Compiler - C# Console Edition
// Converts a supported subset of C into Linux AArch64 assembly.
// No external packages are required.

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;

internal sealed class Token
{
    public string Type { get; }
    public string Value { get; }
    public int Position { get; }

    public Token(string type, string value, int position)
    {
        Type = type;
        Value = value;
        Position = position;
    }
}

internal sealed class Lexer
{
    private readonly string source;
    private int position;

    public Lexer(string source) => this.source = source;

    public List<Token> Tokenize()
    {
        if (source.Contains("#include", StringComparison.Ordinal))
            throw new Exception("Compilation Error: '#include' directives are not permitted.");

        var result = new List<Token>();

        while (position < source.Length)
        {
            char c = source[position];

            if (char.IsWhiteSpace(c))
            {
                position++;
                continue;
            }

            if (c == '/' && Peek(1) == '/')
            {
                position += 2;
                while (position < source.Length && source[position] != '\n')
                    position++;
                continue;
            }

            if (c == '/' && Peek(1) == '*')
            {
                position += 2;
                while (position < source.Length &&
                       !(source[position] == '*' && Peek(1) == '/'))
                    position++;

                if (position >= source.Length)
                    throw new Exception("Unterminated block comment.");

                position += 2;
                continue;
            }

            int start = position;

            if (c == '"')
            {
                position++;
                var value = new StringBuilder();
                bool closed = false;

                while (position < source.Length)
                {
                    char ch = source[position++];

                    if (ch == '"')
                    {
                        closed = true;
                        break;
                    }

                    if (ch == '\\')
                    {
                        if (position >= source.Length)
                            throw new Exception("Incomplete escape sequence in string.");

                        char next = source[position++];
                        switch (next)
                        {
                            case 'n': value.Append('\n'); break;
                            case 't': value.Append('\t'); break;
                            case 'r': value.Append('\r'); break;
                            case '\\': value.Append('\\'); break;
                            case '"': value.Append('"'); break;
                            case '0': value.Append('\0'); break;
                            default: value.Append(next); break;
                        }
                    }
                    else
                    {
                        value.Append(ch);
                    }
                }

                if (!closed)
                    throw new Exception("Unterminated string literal.");

                result.Add(new Token("STRING", value.ToString(), start));
                continue;
            }

            if (char.IsDigit(c))
            {
                while (position < source.Length && char.IsDigit(source[position]))
                    position++;

                result.Add(new Token("INT",
                    source.Substring(start, position - start), start));
                continue;
            }

            if (char.IsLetter(c) || c == '_')
            {
                position++;
                while (position < source.Length &&
                       (char.IsLetterOrDigit(source[position]) || source[position] == '_'))
                    position++;

                result.Add(new Token("IDENT",
                    source.Substring(start, position - start), start));
                continue;
            }

            string two = position + 1 < source.Length
                ? source.Substring(position, 2)
                : "";

            if (new[] { "==", "!=", "<=", ">=" }.Contains(two))
            {
                result.Add(new Token("OP", two, start));
                position += 2;
                continue;
            }

            if ("+-*/%<>=(){};,".Contains(c))
            {
                result.Add(new Token("PUNCT", c.ToString(), start));
                position++;
                continue;
            }

            throw new Exception($"Unexpected character: '{c}' at offset {position}");
        }

        result.Add(new Token("EOF", "", source.Length));
        return result;
    }

    private char Peek(int offset)
    {
        int index = position + offset;
        return index < source.Length ? source[index] : '\0';
    }
}

internal sealed class Expr
{
    public string Kind = "";
    public string Op = "";
    public string Name = "";
    public string Text = "";
    public long Number;
    public Expr? Left;
    public Expr? Right;
    public List<Expr> Arguments = new();
}

internal sealed class Stmt
{
    public string Kind = "";
    public string Name = "";
    public Expr? Expression;
    public Expr? Condition;
    public Expr? Update;
    public Stmt? Initializer;
    public List<Stmt> Body = new();
    public List<Stmt>? ElseBody;
}

internal sealed class Parameter
{
    public string Type = "";
    public string Name = "";
}

internal sealed class Function
{
    public string ReturnType = "";
    public string Name = "";
    public List<Parameter> Parameters = new();
    public Dictionary<string, int> Locals = new();
    public int FrameSize;
    public List<Stmt> Body = new();
}

internal sealed class Parser
{
    private readonly List<Token> tokens;
    private int cursor;
    private readonly Dictionary<string, int> locals = new();
    private int frameOffset = 16;

    public Parser(List<Token> tokens) => this.tokens = tokens;

    private Token Peek(int offset = 0)
    {
        int index = Math.Min(cursor + offset, tokens.Count - 1);
        return tokens[index];
    }

    private Token Take() => tokens[cursor++];

    private bool Match(string value)
    {
        if (Peek().Value != value)
            return false;
        cursor++;
        return true;
    }

    private Token Expect(string value)
    {
        Token token = Take();
        if (token.Value != value)
            throw Error($"Expected '{value}', got '{token.Value}'", token);
        return token;
    }

    private Exception Error(string message, Token? token = null)
    {
        token ??= Peek();
        return new Exception($"{message} at offset {token.Position}");
    }

    private string ExpectIdentifier()
    {
        Token t = Take();
        if (t.Type != "IDENT")
            throw Error($"Expected identifier, got '{t.Value}'", t);
        return t.Value;
    }

    public List<Function> ParseProgram()
    {
        var functions = new List<Function>();

        while (Peek().Type != "EOF")
        {
            Token returnToken = Take();
            if (returnToken.Value != "int" &&
                returnToken.Value != "void" &&
                returnToken.Value != "char")
                throw Error($"Unsupported function return type '{returnToken.Value}'", returnToken);

            var fn = new Function
            {
                ReturnType = returnToken.Value,
                Name = ExpectIdentifier()
            };

            Expect("(");
            if (Peek().Value != ")")
            {
                while (true)
                {
                    Token type = Take();
                    if (type.Value != "int" && type.Value != "char")
                        throw Error($"Unsupported parameter type '{type.Value}'", type);

                    fn.Parameters.Add(new Parameter
                    {
                        Type = type.Value,
                        Name = ExpectIdentifier()
                    });

                    if (Peek().Value == ")")
                        break;
                    Expect(",");
                }
            }

            Expect(")");
            Expect("{");

            locals.Clear();
            frameOffset = 16;

            for (int i = 0; i < fn.Parameters.Count; i++)
            {
                frameOffset += 16;
                locals[fn.Parameters[i].Name] = frameOffset;
            }

            while (Peek().Value != "}")
            {
                if (Peek().Type == "EOF")
                    throw Error("Unexpected end of file; missing '}'");
                fn.Body.Add(ParseStatement());
            }

            Expect("}");
            fn.Locals = new Dictionary<string, int>(locals);
            fn.FrameSize = ((frameOffset + 16 + 15) / 16) * 16;
            functions.Add(fn);
        }

        if (functions.Count == 0)
            throw new Exception("No function definitions found.");

        if (!functions.Any(f => f.Name == "main"))
            throw new Exception("The program must define main().");

        return functions;
    }

    private Expr ParseExpression() => ParseComparison();

    private Expr ParseAssignmentExpression()
    {
        if (Peek().Type == "IDENT" && Peek(1).Value == "=")
        {
            string name = Take().Value;
            Take();
            return new Expr
            {
                Kind = "ASSIGN_EXPR",
                Name = name,
                Left = ParseExpression()
            };
        }

        return ParseExpression();
    }

    private Expr ParseComparison()
    {
        Expr left = ParseAdditive();

        while (new[] { "==", "!=", "<", ">", "<=", ">=" }.Contains(Peek().Value))
        {
            string op = Take().Value;
            left = new Expr
            {
                Kind = "BINARY",
                Op = op,
                Left = left,
                Right = ParseAdditive()
            };
        }

        return left;
    }

    private Expr ParseAdditive()
    {
        Expr left = ParseMultiplicative();

        while (Peek().Value == "+" || Peek().Value == "-")
        {
            string op = Take().Value;
            left = new Expr
            {
                Kind = "BINARY",
                Op = op,
                Left = left,
                Right = ParseMultiplicative()
            };
        }

        return left;
    }

    private Expr ParseMultiplicative()
    {
        Expr left = ParseUnary();

        while (new[] { "*", "/", "%" }.Contains(Peek().Value))
        {
            string op = Take().Value;
            left = new Expr
            {
                Kind = "BINARY",
                Op = op,
                Left = left,
                Right = ParseUnary()
            };
        }

        return left;
    }

    private Expr ParseUnary()
    {
        if (Match("-"))
        {
            return new Expr
            {
                Kind = "UNARY",
                Op = "-",
                Left = ParseUnary()
            };
        }

        return ParsePrimary();
    }

    private Expr ParsePrimary()
    {
        Token token = Take();

        if (token.Type == "INT")
        {
            if (!long.TryParse(token.Value, NumberStyles.Integer,
                               CultureInfo.InvariantCulture, out long number))
                throw Error("Integer literal is out of range", token);

            return new Expr { Kind = "LITERAL", Number = number };
        }

        if (token.Type == "STRING")
            return new Expr { Kind = "STRING", Text = token.Value };

        if (token.Value == "(")
        {
            Expr expression = ParseExpression();
            Expect(")");
            return expression;
        }

        if (token.Type == "IDENT")
        {
            if (Match("("))
            {
                var args = new List<Expr>();

                if (Peek().Value != ")")
                {
                    while (true)
                    {
                        args.Add(ParseExpression());
                        if (Peek().Value == ")")
                            break;
                        Expect(",");
                    }
                }

                Expect(")");
                return new Expr
                {
                    Kind = "CALL",
                    Name = token.Value,
                    Arguments = args
                };
            }

            return new Expr { Kind = "VAR", Name = token.Value };
        }

        throw Error($"Unexpected token in expression: '{token.Value}'", token);
    }

    private Stmt ParseStatement()
    {
        Token token = Peek();

        if (Match("return"))
        {
            Expr? expr = Peek().Value == ";" ? null : ParseExpression();
            Expect(";");
            return new Stmt { Kind = "RETURN", Expression = expr };
        }

        if (Match("if"))
        {
            Expect("(");
            Expr condition = ParseExpression();
            Expect(")");
            var thenBody = ParseBlock();

            List<Stmt>? elseBody = null;
            if (Match("else"))
                elseBody = ParseBlock();

            return new Stmt
            {
                Kind = "IF",
                Condition = condition,
                Body = thenBody,
                ElseBody = elseBody
            };
        }

        if (Match("while"))
        {
            Expect("(");
            Expr condition = ParseExpression();
            Expect(")");
            return new Stmt
            {
                Kind = "WHILE",
                Condition = condition,
                Body = ParseBlock()
            };
        }

        if (Match("for"))
        {
            Expect("(");
            Stmt initializer = ParseStatement();
            Expr condition = ParseExpression();
            Expect(";");
            Expr update = ParseAssignmentExpression();
            Expect(")");
            return new Stmt
            {
                Kind = "FOR",
                Initializer = initializer,
                Condition = condition,
                Update = update,
                Body = ParseBlock()
            };
        }

        if (Peek().Value == "int" || Peek().Value == "char")
        {
            Take();
            string name = ExpectIdentifier();

            if (locals.ContainsKey(name))
                throw Error($"Variable '{name}' is already declared");

            frameOffset += 16;
            locals[name] = frameOffset;

            Expr? initializer = null;
            if (Match("="))
                initializer = ParseExpression();

            Expect(";");
            return new Stmt
            {
                Kind = "VAR_DECL",
                Name = name,
                Expression = initializer
            };
        }

        if (token.Type == "IDENT" && Peek(1).Value == "=")
        {
            string name = Take().Value;
            Take();
            Expr value = ParseExpression();
            Expect(";");
            return new Stmt
            {
                Kind = "ASSIGN",
                Name = name,
                Expression = value
            };
        }

        if (token.Type == "IDENT")
        {
            Expr expression = ParseExpression();
            Expect(";");
            return new Stmt { Kind = "EXPR_STMT", Expression = expression };
        }

        throw Error($"Unrecognized statement: '{token.Value}", token);
    }

    private List<Stmt> ParseBlock()
    {
        Expect("{");
        var body = new List<Stmt>();

        while (Peek().Value != "}")
        {
            if (Peek().Type == "EOF")
                throw Error("Unexpected end of file; missing '}'");
            body.Add(ParseStatement());
        }

        Expect("}");
        return body;
    }
}

internal sealed class Arm64Compiler
{
    private readonly List<(string Label, string Text)> strings = new();
    private readonly Dictionary<string, string> stringLabels = new();
    private int labelCounter;
    private readonly List<string> asm = new();

    private string NewLabel(string prefix = "L") => $".{prefix}_{labelCounter++}";

    private string AddString(string value)
    {
        if (stringLabels.TryGetValue(value, out string? existing))
            return existing;

        string label = $".LC{strings.Count}";
        strings.Add((label, value));
        stringLabels[value] = label;
        return label;
    }

    public string Compile(List<Function> functions)
    {
        asm.Clear();
        asm.Add("/* ARM64 Linux Assembly Generated from C source */");
        asm.Add(".arch armv8-a");
        asm.Add(".text");
        asm.Add("");
        asm.Add("/* Linux sys_exit(93) entry point calling main */");
        asm.Add(".global _start");
        asm.Add("_start:");
        asm.Add("    bl main");
        asm.Add("    mov x8, #93");
        asm.Add("    svc #0");
        asm.Add("");

        foreach (Function fn in functions)
            GenerateFunction(fn);

        asm.Add(EmitBuiltinPrintf());

        if (strings.Count > 0)
        {
            asm.Add(".section .rodata");
            foreach (var item in strings)
            {
                asm.Add(item.Label + ":");
                asm.Add("    .string \"" + EscapeString(item.Text) + "\"");
            }
        }

        return string.Join(Environment.NewLine, asm);
    }

    private void GenerateFunction(Function fn)
    {
        asm.Add($".global {fn.Name}");
        asm.Add($".type {fn.Name}, %function");
        asm.Add($"{fn.Name}:");
        asm.Add($"    stp x29, x30, [sp, -{fn.FrameSize}]!");
        asm.Add("    mov x29, sp");

        for (int i = 0; i < fn.Parameters.Count && i < 8; i++)
        {
            string name = fn.Parameters[i].Name;
            if (fn.Locals.TryGetValue(name, out int offset))
                asm.Add($"    str x{i}, [x29, #{offset}]");
        }

        foreach (Stmt statement in fn.Body)
            GenerateStatement(fn, statement);

        asm.Add("    mov x0, #0");
        asm.Add("    mov sp, x29");
        asm.Add($"    ldp x29, x30, [sp], #{fn.FrameSize}");
        asm.Add("    ret");
        asm.Add("");
    }

    private void GenerateExpression(Function fn, Expr expr)
    {
        switch (expr.Kind)
        {
            case "LITERAL":
                EmitMoveImmediate("x0", expr.Number);
                break;

            case "STRING":
                string label = AddString(expr.Text);
                asm.Add($"    adrp x0, {label}");
                asm.Add($"    add x0, x0, :lo12:{label}");
                break;

            case "VAR":
                if (!fn.Locals.TryGetValue(expr.Name, out int offset))
                    throw new Exception($"Undeclared variable: {expr.Name}");
                asm.Add($"    ldr x0, [x29, #{offset}]");
                break;

            case "UNARY":
                GenerateExpression(fn, expr.Left!);
                asm.Add("    neg x0, x0");
                break;

            case "BINARY":
                GenerateExpression(fn, expr.Left!);
                asm.Add("    str x0, [sp, #-16]!");
                GenerateExpression(fn, expr.Right!);
                asm.Add("    mov x1, x0");
                asm.Add("    ldr x0, [sp], #16");

                switch (expr.Op)
                {
                    case "+": asm.Add("    add x0, x0, x1"); break;
                    case "-": asm.Add("    sub x0, x0, x1"); break;
                    case "*": asm.Add("    mul x0, x0, x1"); break;
                    case "/": asm.Add("    sdiv x0, x0, x1"); break;
                    case "%":
                        asm.Add("    sdiv x2, x0, x1");
                        asm.Add("    msub x0, x2, x1, x0");
                        break;
                    case "==": EmitCompare("eq"); break;
                    case "!=": EmitCompare("ne"); break;
                    case "<": EmitCompare("lt"); break;
                    case ">": EmitCompare("gt"); break;
                    case "<=": EmitCompare("le"); break;
                    case ">=": EmitCompare("ge"); break;
                    default: throw new Exception($"Unsupported operator: {expr.Op}");
                }
                break;

            case "ASSIGN_EXPR":
                if (!fn.Locals.TryGetValue(expr.Name, out int assignOffset))
                    throw new Exception($"Unknown variable: {expr.Name}");
                GenerateExpression(fn, expr.Left!);
                asm.Add($"    str x0, [x29, #{assignOffset}]");
                break;

            case "CALL":
                if (expr.Arguments.Count > 8)
                    throw new Exception("A maximum of 8 function arguments is supported.");

                foreach (Expr argument in expr.Arguments)
                {
                    GenerateExpression(fn, argument);
                    asm.Add("    str x0, [sp, #-16]!");
                }

                for (int i = expr.Arguments.Count - 1; i >= 0; i--)
                    asm.Add($"    ldr x{i}, [sp], #16");

                asm.Add(expr.Name == "printf" ? "    bl __sys_printf" : $"    bl {expr.Name}");
                break;

            default:
                throw new Exception($"Unknown expression node: {expr.Kind}");
        }
    }

    private void EmitCompare(string condition)
    {
        asm.Add("    cmp x0, x1");
        asm.Add($"    cset x0, {condition}");
    }

    private void EmitMoveImmediate(string register, long value)
    {
        if (value >= 0 && value <= 65535)
        {
            asm.Add($"    mov {register}, #{value}");
            return;
        }

        ulong bits = unchecked((ulong)value);
        ushort[] chunks = new ushort[4];
        for (int i = 0; i < 4; i++)
            chunks[i] = (ushort)((bits >> (i * 16)) & 0xffff);

        bool first = true;
        for (int i = 0; i < 4; i++)
        {
            if (chunks[i] == 0 && !first)
                continue;

            if (first)
            {
                asm.Add($"    movz {register}, #{chunks[i]}, lsl #{i * 16}");
                first = false;
            }
            else
            {
                asm.Add($"    movk {register}, #{chunks[i]}, lsl #{i * 16}");
            }
        }

        if (first)
            asm.Add($"    mov {register}, #0");
    }

    private void GenerateStatement(Function fn, Stmt stmt)
    {
        switch (stmt.Kind)
        {
            case "RETURN":
                if (stmt.Expression != null)
                    GenerateExpression(fn, stmt.Expression);
                else
                    asm.Add("    mov x0, #0");
                EmitEpilogue(fn);
                break;

            case "VAR_DECL":
                if (stmt.Expression != null)
                {
                    GenerateExpression(fn, stmt.Expression);
                    asm.Add($"    str x0, [x29, #{fn.Locals[stmt.Name]}]");
                }
                break;

            case "ASSIGN":
                if (!fn.Locals.TryGetValue(stmt.Name, out int offset))
                    throw new Exception($"Unknown variable: {stmt.Name}");
                GenerateExpression(fn, stmt.Expression!);
                asm.Add($"    str x0, [x29, #{offset}]");
                break;

            case "EXPR_STMT":
                GenerateExpression(fn, stmt.Expression!);
                break;

            case "IF":
            {
                string elseLabel = NewLabel("else");
                string endLabel = NewLabel("endif");
                GenerateExpression(fn, stmt.Condition!);
                asm.Add("    cmp x0, #0");
                asm.Add($"    beq {(stmt.ElseBody != null ? elseLabel : endLabel)}");

                foreach (Stmt child in stmt.Body)
                    GenerateStatement(fn, child);

                if (stmt.ElseBody != null)
                {
                    asm.Add($"    b {endLabel}");
                    asm.Add(elseLabel + ":");
                    foreach (Stmt child in stmt.ElseBody)
                        GenerateStatement(fn, child);
                }

                asm.Add(endLabel + ":");
                break;
            }

            case "WHILE":
            {
                string start = NewLabel("while_start");
                string end = NewLabel("while_end");
                asm.Add(start + ":");
                GenerateExpression(fn, stmt.Condition!);
                asm.Add("    cmp x0, #0");
                asm.Add($"    beq {end}");
                foreach (Stmt child in stmt.Body)
                    GenerateStatement(fn, child);
                asm.Add($"    b {start}");
                asm.Add(end + ":");
                break;
            }

            case "FOR":
            {
                string start = NewLabel("for_start");
                string end = NewLabel("for_end");
                GenerateStatement(fn, stmt.Initializer!);
                asm.Add(start + ":");
                GenerateExpression(fn, stmt.Condition!);
                asm.Add("    cmp x0, #0");
                asm.Add($"    beq {end}");
                foreach (Stmt child in stmt.Body)
                    GenerateStatement(fn, child);
                GenerateExpression(fn, stmt.Update!);
                asm.Add($"    b {start}");
                asm.Add(end + ":");
                break;
            }

            default:
                throw new Exception($"Unknown statement: {stmt.Kind}");
        }
    }

    private void EmitEpilogue(Function fn)
    {
        asm.Add("    mov sp, x29");
        asm.Add($"    ldp x29, x30, [sp], #{fn.FrameSize}");
        asm.Add("    ret");
    }

    private static string EscapeString(string value)
    {
        var result = new StringBuilder();
        foreach (char c in value)
        {
            switch (c)
            {
                case '\\': result.Append("\\\\"); break;
                case '"': result.Append("\\\""); break;
                case '\n': result.Append("\\n"); break;
                case '\t': result.Append("\\t"); break;
                case '\r': result.Append("\\r"); break;
                case '\0': result.Append("\\0"); break;
                default:
                    if (c < 32 || c > 126)
                        result.Append("\\x").Append(((int)c).ToString("x2"));
                    else
                        result.Append(c);
                    break;
            }
        }
        return result.ToString();
    }

    private static string EmitBuiltinPrintf()
    {
        return @"
/* =========================================================================
 * Built-in printf using native ARM64 Linux syscalls (sys_write: 64)
 * Supports %d, %s, %% and ordinary characters.
 * ========================================================================= */
.global __sys_printf
__sys_printf:
    stp x29, x30, [sp, -160]!
    mov x29, sp
    stp x19, x20, [x29, #16]
    stp x21, x22, [x29, #32]
    stp x23, x24, [x29, #48]
    stp x1, x2, [x29, #64]
    stp x3, x4, [x29, #80]
    stp x5, x6, [x29, #96]
    str x7, [x29, #112]
    mov x19, x0
    add x20, x29, #64

.Lprint_loop:
    ldrb w21, [x19], #1
    cbz w21, .Lprint_done
    cmp w21, #'%'
    beq .Lprint_fmt

    sub sp, sp, #16
    strb w21, [sp]
    mov x0, #1
    mov x1, sp
    mov x2, #1
    mov x8, #64
    svc #0
    add sp, sp, #16
    b .Lprint_loop

.Lprint_fmt:
    ldrb w21, [x19], #1
    cmp w21, #'%'
    beq .Lprint_percent
    cmp w21, #'s'
    beq .Lprint_str
    cmp w21, #'d'
    beq .Lprint_num
    b .Lprint_loop

.Lprint_percent:
    sub sp, sp, #16
    mov w22, #'%'
    strb w22, [sp]
    mov x0, #1
    mov x1, sp
    mov x2, #1
    mov x8, #64
    svc #0
    add sp, sp, #16
    b .Lprint_loop

.Lprint_str:
    ldr x22, [x20], #8
    mov x0, x22
    bl __sys_strlen
    mov x2, x0
    mov x1, x22
    mov x0, #1
    mov x8, #64
    svc #0
    b .Lprint_loop

.Lprint_num:
    ldr x22, [x20], #8
    mov x0, x22
    bl __sys_print_int
    b .Lprint_loop

.Lprint_done:
    ldp x23, x24, [x29, #48]
    ldp x21, x22, [x29, #32]
    ldp x19, x20, [x29, #16]
    ldp x29, x30, [sp], #160
    ret

__sys_strlen:
    mov x1, x0
.Lstrlen_loop:
    ldrb w2, [x1]
    cbz w2, .Lstrlen_done
    add x1, x1, #1
    b .Lstrlen_loop
.Lstrlen_done:
    sub x0, x1, x0
    ret

__sys_print_int:
    stp x29, x30, [sp, -64]!
    mov x29, sp
    mov x1, x0
    add x2, x29, #48
    mov w3, #0
    strb w3, [x2]
    cmp x1, #0
    bne .Lnum_nonzero
    sub x2, x2, #1
    mov w3, #'0'
    strb w3, [x2]
    b .Lnum_write

.Lnum_nonzero:
    mov x4, #0
    cmp x1, #0
    bge .Lnum_loop
    neg x1, x1
    mov x4, #1

.Lnum_loop:
    cbz x1, .Lnum_check_neg
    mov x5, #10
    udiv x6, x1, x5
    msub x7, x6, x5, x1
    add w7, w7, #'0'
    sub x2, x2, #1
    strb w7, [x2]
    mov x1, x6
    b .Lnum_loop

.Lnum_check_neg:
    cbz x4, .Lnum_write
    sub x2, x2, #1
    mov w3, #'-'
    strb w3, [x2]

.Lnum_write:
    add x3, x29, #48
    sub x3, x3, x2
    mov x0, #1
    mov x1, x2
    mov x2, x3
    mov x8, #64
    svc #0
    ldp x29, x30, [sp], #64
    ret
";
    }
}

internal static class Program
{
    private static readonly Dictionary<string, string> Examples = new()
    {
        ["hello"] = @"int main() {
    printf(""Hello from bare-metal ARM64!\n"");
    printf(""Exit code will be %d.\n"", 0);
    return 0;
}",
        ["fibonacci"] = @"int main() {
    int n = 10;
    int a = 0;
    int b = 1;
    int i = 0;

    printf(""First 10 Fibonacci numbers:\n"");
    while (i < n) {
        printf(""%d\n"", a);
        int next = a + b;
        a = b;
        b = next;
        i = i + 1;
    }
    return 0;
}",
        ["math"] = @"int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    int val = 5;
    int res = factorial(val);
    printf(""Factorial of %d is %d\n"", val, res);
    return 0;
}",
        ["loop"] = @"int main() {
    int i = 1;
    for (i = 1; i <= 5; i = i + 1) {
        printf(""Step %d squared = %d\n"", i, i * i);
    }
    return 0;
}"
    };

    private static int Main(string[] args)
    {
        Console.BackgroundColor = ConsoleColor.White;
        Console.ForegroundColor= ConsoleColor.Black;
        Console.Clear();
        try
        {
            if (args.Length == 0 || args.Contains("--help") || args.Contains("-h"))
            {
                ShowHelp();
                return 0;
            }

            if (args.Length == 2 && args[0] == "--example")
            {
                if (!Examples.TryGetValue(args[1], out string? example))
                    throw new Exception("Unknown example. Use hello, fibonacci, math or loop.");

                Console.Write(example);
                return 0;
            }

            string inputPath = args[0];
            string? outputPath = args.Length >= 2 ? args[1] : null;

            string source = File.ReadAllText(inputPath, Encoding.UTF8);
            var lexer = new Lexer(source);
            List<Token> tokens = lexer.Tokenize();
            var parser = new Parser(tokens);
            List<Function> functions = parser.ParseProgram();
            string assembly = new Arm64Compiler().Compile(functions);

            if (!string.IsNullOrWhiteSpace(outputPath))
            {
                File.WriteAllText(outputPath, assembly + Environment.NewLine,
                                  new UTF8Encoding(false));
                Console.WriteLine($"Assembly generated: {Path.GetFullPath(outputPath)}");
            }
            else
            {
                Console.WriteLine(assembly);
            }

            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("Compilation Error: " + ex.Message);
            return 1;
        }
    }

    private static void ShowHelp()
    {
        Console.WriteLine("C to ARM64 Compiler - C# Console Edition");
        Console.WriteLine();
        Console.WriteLine("Usage:");
        Console.WriteLine("  CToARM64 input.c output.s");
        Console.WriteLine("  CToARM64 input.c");
        Console.WriteLine("  CToARM64 --example hello");
        Console.WriteLine("  CToARM64 --example fibonacci");
        Console.WriteLine("  CToARM64 --example math");
        Console.WriteLine("  CToARM64 --example loop");
        Console.WriteLine();
        Console.WriteLine("If output.s is omitted, assembly is written to the console.");
        Console.WriteLine("Supported C subset: int/char/void functions, local variables,");
        Console.WriteLine("arithmetic, comparisons, calls, if/else, while, for and printf.");
        Console.WriteLine("No #include directives or external C libraries are required.");
    }
}
