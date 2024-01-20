const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const TokenType = Scanner.TokenType;
const Token = Scanner.Token;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const debug = @import("./debug.zig");

const errout = std.io.getStdErr().writer();
var compilingChunk: *Chunk = undefined;

pub fn compile(source: []const u8, chunk: *Chunk) bool {
    compilingChunk = chunk;
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    var compiler = Compiler.init(&parser);
    parser.advance();
    compiler.expression();
    // parser.consume(TokenType.TOKEN_EOF, "Expect end of expression.");
    compiler.deinit();
    return !parser.hadError;
}

const Parser = struct {
    previous: Token,
    current: Token,
    hadError: bool = false,
    panicMode: bool = false,
    scanner: *Scanner,

    pub fn init(scanner: *Scanner) Parser {
        return Parser{ .scanner = scanner, .current = undefined, .previous = undefined };
    }

    /// advance to the next token and reports an error if found.
    pub fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.type != TokenType.TOKEN_ERROR) break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    /// consume a token only if the current token matches the type
    pub fn consume(self: *Parser, ttype: TokenType, message: []const u8) void {
        if (self.current.type == ttype) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn err(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;

        errout.print("[line {d}] Error", .{token.line}) catch unreachable;

        if (token.type == TokenType.TOKEN_EOF) {
            errout.print(" at end", .{}) catch unreachable;
        } else if (token.type == TokenType.TOKEN_ERROR) {
            // nothing
        } else {
            errout.print(" at {d}", .{token.lexeme.len}) catch unreachable;
        }
        errout.print(": {s}\n", .{message}) catch unreachable;

        self.hadError = true;
    }
};

const ParseFn = *const fn (compiler: *Compiler) void;

const Compiler = struct {
    parser: *Parser,

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?ParseFn,
        precedence: Precedence,

        pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
            return ParseRule{ .prefix = prefix, .infix = infix, .precedence = precedence };
        }
    };

    const Precedence = enum {
        PREC_NONE,
        PREC_ASSIGNMENT, // =
        PREC_OR, // or
        PREC_AND, // and
        PREC_EQUALITY, // == !=
        PREC_COMPARISON, // < > <= >=
        PREC_TERM, // + -
        PREC_FACTOR, // * /
        PREC_UNARY, // ! -
        PREC_CALL, // . ()
        PREC_PRIMARY,
    };

    pub fn init(parser: *Parser) Compiler {
        return Compiler{ .parser = parser };
    }

    pub fn deinit(self: *Compiler) void {
        self.emitReturn();

        if (debug.debug_print_code and !self.parser.hadError) {
            debug.disassembleChunk(self.currentChunk(), "code");
        }
    }

    fn emitOp(self: *Compiler, opcode: OpCode) void {
        self.currentChunk().writeOpCode(opcode, self.parser.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit opcode", .{e});
            std.process.exit(1);
        };
    }

    pub fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().writeCode(byte, self.parser.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte", .{e});
            std.process.exit(1);
        };
    }

    fn emitUnartyOp(self: *Compiler, opcode: OpCode, byte: u8) void {
        self.emitOp(opcode);
        self.emitByte(byte);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitUnartyOp(OpCode.OP_CONSTANT, self.makeConstant(value));
    }

    fn emitReturn(self: *Compiler) void {
        self.emitOp(OpCode.OP_RETURN);
    }

    fn currentChunk(self: *Compiler) *Chunk {
        _ = self;
        return compilingChunk;
    }

    fn makeConstant(self: *Compiler, value: Value) u8 {
        const constant = self.currentChunk().addConstant(value) catch 0;
        if (constant > std.math.maxInt(u8)) {
            std.debug.print("too mant constants in one chunk", .{});
            return 0;
        }

        return @intCast(constant);
    }

    fn expression(self: *Compiler) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    fn number(self: *Compiler) void {
        if (std.fmt.parseFloat(f64, self.parser.previous.lexeme)) |value| {
            self.emitConstant(Value.fromNumber(value));
        } else |e| switch (e) {
            std.fmt.ParseFloatError.InvalidCharacter => {
                std.debug.print("cound not parse number", .{});
            },
        }
    }

    fn literal(self: *Compiler) void {
        switch (self.parser.previous.type) {
            TokenType.TOKEN_FALSE => self.emitOp(OpCode.OP_FALSE),
            TokenType.TOKEN_TRUE => self.emitOp(OpCode.OP_TRUE),
            TokenType.TOKEN_NIL => self.emitOp(OpCode.OP_NIL),
            else => unreachable,
        }
    }
    // assuming we consumed the initial '(', we call the expression function and expect to have the closing ')' after.
    fn grouping(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Compiler) void {
        const operatorType = self.parser.previous.type;

        // compile the operand.
        self.parsePrecedence(.PREC_UNARY);

        // emit the operator instruction.
        switch (operatorType) {
            TokenType.TOKEN_MINUS => self.emitOp(OpCode.OP_NEGATE),
            TokenType.TOKEN_BANG => self.emitOp(OpCode.OP_NOT),
            else => {
                unreachable;
            },
        }
    }

    fn binary(self: *Compiler) void {
        const operatorType = self.parser.previous.type;
        const rule = self.getRule(operatorType);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operatorType) {
            .TOKEN_PLUS => self.emitOp(OpCode.OP_ADD),
            .TOKEN_MINUS => self.emitOp(OpCode.OP_SUBTRACT),
            .TOKEN_STAR => self.emitOp(OpCode.OP_MULTIPLY),
            .TOKEN_SLASH => self.emitOp(OpCode.OP_DIVIDE),
            else => {
                unreachable;
            },
        }
    }

    fn getRule(self: *Compiler, ttoken: TokenType) ParseRule {
        _ = self;
        const rule = switch (ttoken) {
            TokenType.TOKEN_LEFT_PAREN => comptime ParseRule.init(Compiler.grouping, null, Precedence.PREC_NONE),
            TokenType.TOKEN_RIGHT_PAREN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_LEFT_BRACE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_RIGHT_BRACE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_COMMA => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_DOT => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_MINUS => comptime ParseRule.init(Compiler.unary, Compiler.binary, Precedence.PREC_TERM),
            TokenType.TOKEN_PLUS => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_TERM),
            TokenType.TOKEN_SEMICOLON => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_SLASH => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_FACTOR),
            TokenType.TOKEN_STAR => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_FACTOR),
            TokenType.TOKEN_BANG => comptime ParseRule.init(Compiler.unary, null, Precedence.PREC_NONE),
            TokenType.TOKEN_BANG_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EQUAL_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_GREATER => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_GREATER_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_LESS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_LESS_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_IDENTIFIER => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_STRING => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_NUMBER => comptime ParseRule.init(Compiler.number, null, Precedence.PREC_NONE),
            TokenType.TOKEN_AND => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_CLASS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_ELSE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FALSE => comptime ParseRule.init(Compiler.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FOR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FUN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_IF => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_NIL => comptime ParseRule.init(Compiler.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_OR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_PRINT => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_RETURN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_SUPER => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_THIS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_TRUE => comptime ParseRule.init(Compiler.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_VAR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_WHILE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_ERROR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EOF => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
        };

        return rule;
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) void {
        self.parser.advance();
        const prefixRule = self.getRule(self.parser.previous.type).prefix orelse {
            self.parser.err("Expect expression");
            return;
        };

        prefixRule(self);

        while (@intFromEnum(precedence) <= @intFromEnum(self.getRule(self.parser.current.type).precedence)) {
            self.parser.advance();
            const infixRule = self.getRule(self.parser.previous.type).infix orelse {
                self.parser.err("Expect expression");
                return;
            };

            infixRule(self);
        }
    }
};
