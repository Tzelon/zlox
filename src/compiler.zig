const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const TokenType = Scanner.TokenType;
const Token = Scanner.Token;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const VM = @import("./vm.zig").VM;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const debug = @import("./debug.zig");

const errout = std.io.getStdErr().writer();
var compilingChunk: *Chunk = undefined;

pub fn compile(vm: *VM, source: []const u8, chunk: *Chunk) bool {
    compilingChunk = chunk;
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    var compiler = Compiler.init(vm, &parser);
    parser.advance();
    while (!parser.match(.TOKEN_EOF)) {
        compiler.declaration();
    }
    parser.consume(TokenType.TOKEN_EOF, "Expect end of expression.");
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

        while (self.scanner.scanToken()) |token| {
            self.current = token;
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

    /// consume the token if the current token has the given type
    fn match(self: *Parser, ttype: TokenType) bool {
        if (!self.check(ttype)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, ttype: TokenType) bool {
        return self.current.type == ttype;
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

const ParseFn = *const fn (compiler: *Compiler, can_assign: bool) void;

const Compiler = struct {
    parser: *Parser,
    vm: *VM,

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

    pub fn init(vm: *VM, parser: *Parser) Compiler {
        return Compiler{ .vm = vm, .parser = parser };
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

    fn emitUnaryOp(self: *Compiler, opcode: OpCode, byte: u8) void {
        self.emitOp(opcode);
        self.emitByte(byte);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitUnaryOp(OpCode.OP_CONSTANT, self.makeConstant(value));
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

    fn declaration(self: *Compiler) void {
        if (self.parser.match(TokenType.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.parser.panicMode) self.synchronize();
    }

    fn statement(self: *Compiler) void {
        if (self.parser.match(TokenType.TOKEN_PRINT)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn varDeclaration(self: *Compiler) void {
        const global = self.parseVariable("Expect variable name");

        if (self.parser.match(TokenType.TOKEN_EQUAL)) {
            self.expression();
        } else {
            self.emitOp(OpCode.OP_NIL);
        }
        self.parser.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");

        self.defineVariable(global);
    }

    fn expressionStatement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");
        self.emitOp(OpCode.OP_POP);
    }

    fn printStatement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");
        self.emitOp(OpCode.OP_PRINT);
    }

    fn synchronize(self: *Compiler) void {
        self.parser.panicMode = false;

        while (self.parser.current.type != TokenType.TOKEN_EOF) {
            if (self.parser.previous.type == TokenType.TOKEN_SEMICOLON) return;
            switch (self.parser.current.type) {
                .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_WHILE, .TOKEN_IF, .TOKEN_PRINT, .TOKEN_RETURN => return,
                else => self.parser.advance(),
            }
        }
    }

    fn number(self: *Compiler, _: bool) void {
        if (std.fmt.parseFloat(f64, self.parser.previous.lexeme)) |value| {
            self.emitConstant(Value.fromNumber(value));
        } else |e| switch (e) {
            std.fmt.ParseFloatError.InvalidCharacter => {
                std.debug.print("cound not parse number", .{});
            },
        }
    }

    fn string(self: *Compiler, _: bool) void {
        const lexeme = self.parser.previous.lexeme;
        const value = Obj.String.copy(self.vm, lexeme[1 .. lexeme.len - 1]);
        self.emitConstant(Value.fromObj(&value.obj));
    }

    fn variable(self: *Compiler, can_assing: bool) void {
        self.namedVariable(&self.parser.previous, can_assing);
    }

    fn literal(self: *Compiler, _: bool) void {
        switch (self.parser.previous.type) {
            TokenType.TOKEN_FALSE => self.emitOp(OpCode.OP_FALSE),
            TokenType.TOKEN_TRUE => self.emitOp(OpCode.OP_TRUE),
            TokenType.TOKEN_NIL => self.emitOp(OpCode.OP_NIL),
            else => unreachable,
        }
    }
    // assuming we consumed the initial '(', we call the expression function and expect to have the closing ')' after.
    fn grouping(self: *Compiler, _: bool) void {
        self.expression();
        self.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Compiler, _: bool) void {
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

    fn binary(self: *Compiler, _: bool) void {
        const operatorType = self.parser.previous.type;
        const rule = self.getRule(operatorType);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operatorType) {
            .TOKEN_PLUS => self.emitOp(OpCode.OP_ADD),
            .TOKEN_MINUS => self.emitOp(OpCode.OP_SUBTRACT),
            .TOKEN_STAR => self.emitOp(OpCode.OP_MULTIPLY),
            .TOKEN_SLASH => self.emitOp(OpCode.OP_DIVIDE),
            .TOKEN_BANG_EQUAL => {
                self.emitOp(OpCode.OP_EQUAL);
                self.emitOp(OpCode.OP_NOT);
            },
            .TOKEN_EQUAL_EQUAL => self.emitOp(OpCode.OP_EQUAL),
            .TOKEN_GREATER => self.emitOp(OpCode.OP_GREATER),
            .TOKEN_GREATER_EQUAL => {
                self.emitOp(OpCode.OP_LESS);
                self.emitOp(OpCode.OP_NOT);
            },
            .TOKEN_LESS => self.emitOp(OpCode.OP_LESS),
            .TOKEN_LESS_EQUAL => {
                self.emitOp(OpCode.OP_GREATER);
                self.emitOp(OpCode.OP_NOT);
            },
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
            TokenType.TOKEN_BANG_EQUAL => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_EQUALITY),
            TokenType.TOKEN_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EQUAL_EQUAL => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_EQUALITY),
            TokenType.TOKEN_GREATER => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_GREATER_EQUAL => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_LESS => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_LESS_EQUAL => comptime ParseRule.init(null, Compiler.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_IDENTIFIER => comptime ParseRule.init(Compiler.variable, null, Precedence.PREC_NONE),
            TokenType.TOKEN_STRING => comptime ParseRule.init(Compiler.string, null, Precedence.PREC_NONE),
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

    fn namedVariable(self: *Compiler, name: *Token, can_assign: bool) void {
        var arg = self.identifierConstant(name);

        // check if this is a setter or a getter
        if (can_assign and self.parser.match(TokenType.TOKEN_EQUAL)) {
            self.expression();
            self.emitUnaryOp(OpCode.OP_SET_GLOBAL, arg);
        } else {
            self.emitUnaryOp(OpCode.OP_GET_GLOBAL, arg);
        }
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) void {
        self.parser.advance();
        const prefixRule = self.getRule(self.parser.previous.type).prefix orelse {
            self.parser.err("Expect expression");
            return;
        };

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);

        prefixRule(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(self.getRule(self.parser.current.type).precedence)) {
            self.parser.advance();
            const infixRule = self.getRule(self.parser.previous.type).infix orelse {
                self.parser.err("Expect expression");
                return;
            };

            infixRule(self, can_assign);
        }

        if (can_assign and self.parser.match(TokenType.TOKEN_EQUAL)) {
            self.parser.err("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Compiler, errorMessage: []const u8) u8 {
        self.parser.consume(TokenType.TOKEN_IDENTIFIER, errorMessage);
        return self.identifierConstant(&self.parser.previous);
    }

    fn defineVariable(self: *Compiler, global: u8) void {
        self.emitUnaryOp(OpCode.OP_DEFINE_GLOBAL, global);
    }

    fn identifierConstant(self: *Compiler, name: *Token) u8 {
        const identifier = Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(Value.fromObj(&identifier.obj));
    }
};
