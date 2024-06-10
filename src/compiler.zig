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

const UNIT8_COUNT = std.math.maxInt(u8) + 1;
const errout = std.io.getStdErr().writer();
const FunctionType = enum { TYPE_FUNCTION, TYPE_SCRIPT };
const CompileError = error{ CompileError, TooManyConstants };
var compilingChunk: *Chunk = undefined;

pub fn compile(vm: *VM, source: []const u8) CompileError!*Obj.Function {
    // scanner scan the source and creates tokens
    var scanner = Scanner.init(source);

    var compiler = Compiler.init(vm, .TYPE_SCRIPT, null);

    var parser = Parser.init(vm, &scanner, &compiler);

    // start the program parsing
    parser.advance();
    while (!parser.match(.TOKEN_EOF)) {
        parser.declaration();
    }
    parser.consume(TokenType.TOKEN_EOF, "Expect end of expression.");

    const function = parser.endCompiler();
    return if (parser.hadError) CompileError.CompileError else function;
}

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

/// parser is doing two things
/// 1. get the next token from the scanner and make sense of the program.
/// 2. generate code for the vm to use.
const Parser = struct {
    vm: *VM,
    compiler: *Compiler,
    scanner: *Scanner,
    previous: Token,
    current: Token,
    hadError: bool = false,
    panicMode: bool = false,

    pub fn init(vm: *VM, scanner: *Scanner, compiler: *Compiler) Parser {
        return Parser{ .scanner = scanner, .compiler = compiler, .vm = vm, .current = undefined, .previous = undefined };
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

    /// end the compiler and return to the enclosing compiler
    pub fn endCompiler(self: *Parser) *Obj.Function {
        self.emitReturn();
        const func = self.compiler.function;

        if (debug.debug_print_code and !self.hadError) {
            debug.disassembleChunk(self.currentChunk(), if (func.name) |name| name.chars else "<script>");
        }

        if (self.compiler.enclosing) |enclosing| self.compiler = enclosing;
        return func;
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

    fn emitOp(self: *Parser, opcode: OpCode) void {
        self.currentChunk().writeOpCode(opcode, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit opcode", .{e});
            std.process.exit(1);
        };
    }

    pub fn emitByte(self: *Parser, byte: u8) void {
        self.currentChunk().writeCode(byte, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte", .{e});
            std.process.exit(1);
        };
    }

    pub fn emitLoop(self: *Parser, loopStart: usize) void {
        self.emitOp(OpCode.OP_LOOP);

        const offset = self.currentChunk().code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.err("Loop body too large.");
        }

        self.emitByte(@intCast((offset >> 8) & 0xff));
        self.emitByte(@intCast(offset & 0xff));
    }

    pub fn emitJump(self: *Parser, instruction: OpCode) usize {
        self.emitOp(instruction);
        // dummy operands that will be patched later.
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn emitUnaryOp(self: *Parser, opcode: OpCode, byte: u8) void {
        self.emitOp(opcode);
        self.emitByte(byte);
    }

    fn emitConstant(self: *Parser, value: Value) void {
        self.emitUnaryOp(OpCode.OP_CONSTANT, self.makeConstant(value));
    }

    fn patchJump(self: *Parser, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emitReturn(self: *Parser) void {
        self.emitOp(OpCode.OP_NIL);
        self.emitOp(OpCode.OP_RETURN);
    }

    fn currentChunk(self: *Parser) *Chunk {
        return &self.compiler.function.chunk;
    }

    fn makeConstant(self: *Parser, value: Value) u8 {
        const constant = self.currentChunk().addConstant(value) catch 0;
        if (constant > std.math.maxInt(u8)) {
            std.debug.print("too mant constants in one chunk", .{});
            return 0;
        }

        return @intCast(constant);
    }

    fn expression(self: *Parser) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    fn declaration(self: *Parser) void {
        if (self.match(TokenType.TOKEN_FUN)) {
            self.funDeclaration();
        } else if (self.match(TokenType.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    fn statement(self: *Parser) void {
        if (self.match(TokenType.TOKEN_PRINT)) {
            self.printStatement();
        } else if (self.match(TokenType.TOKEN_IF)) {
            self.ifStatement();
        } else if (self.match(TokenType.TOKEN_RETURN)) {
            self.returnStatement();
        } else if (self.match(TokenType.TOKEN_WHILE)) {
            self.whileStatement();
        } else if (self.match(TokenType.TOKEN_FOR)) {
            self.forStatement();
        } else if (self.match(TokenType.TOKEN_LEFT_BRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn block(self: *Parser) void {
        while (!self.check(TokenType.TOKEN_RIGHT_BRACE) and !self.check(TokenType.TOKEN_EOF)) {
            self.declaration();
        }

        self.consume(TokenType.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn funDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect function name");
        self.markInitialized();
        self.function(.TYPE_FUNCTION);
        self.defineVariable(global);
    }

    fn varDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect variable name");

        if (self.match(TokenType.TOKEN_EQUAL)) {
            self.expression();
        } else {
            self.emitOp(OpCode.OP_NIL);
        }
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");

        self.defineVariable(global);
    }

    fn expressionStatement(self: *Parser) void {
        self.expression();
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");
        self.emitOp(OpCode.OP_POP);
    }

    fn ifStatement(self: *Parser) void {
        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const thenJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        self.emitOp(OpCode.OP_POP);
        self.statement();

        const elseJump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(thenJump);
        self.emitOp(OpCode.OP_POP);

        if (self.match(TokenType.TOKEN_ELSE)) {
            self.statement();
        }

        self.patchJump(elseJump);
    }

    fn forStatement(self: *Parser) void {
        self.beginScope();
        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        //initializer
        if (self.match(TokenType.TOKEN_SEMICOLON)) {
            // no initializer
        } else if (self.match(TokenType.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.items.len;
        var exitJump: usize = undefined;

        // condition
        if (!self.match(TokenType.TOKEN_SEMICOLON)) {
            self.expression();
            self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

            // jump out of the loop if the condition is false.
            exitJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
            self.emitOp(OpCode.OP_POP);
        }

        // increment
        if (!self.match(TokenType.TOKEN_RIGHT_PAREN)) {
            const bodyJump = self.emitJump(OpCode.OP_JUMP);
            const incrementStart = self.currentChunk().code.items.len;
            self.expression();
            self.emitOp(OpCode.OP_POP);

            self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

            self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(bodyJump);
        }

        self.statement();
        self.emitLoop(loopStart);

        // check that we have conditional clause
        if (exitJump != undefined) {
            self.patchJump(exitJump);
            self.emitOp(OpCode.OP_POP);
        }

        self.endScope();
    }

    fn returnStatement(self: *Parser) void {
        if (self.compiler.function_type == .TYPE_SCRIPT) {
            self.err("Can't return from top-level code.");
        }

        if (self.match(TokenType.TOKEN_SEMICOLON)) {
            self.emitReturn();
        } else {
            self.expression();
            self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after return value");
            self.emitOp(OpCode.OP_RETURN);
        }
    }

    fn whileStatement(self: *Parser) void {
        const loopStart = self.currentChunk().code.items.len;
        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exitJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        self.emitOp(OpCode.OP_POP);
        self.statement();
        self.emitLoop(loopStart);

        self.patchJump(exitJump);
        self.emitOp(OpCode.OP_POP);
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        self.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after value.");
        self.emitOp(OpCode.OP_PRINT);
    }

    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (self.current.type != TokenType.TOKEN_EOF) {
            if (self.previous.type == TokenType.TOKEN_SEMICOLON) return;
            switch (self.current.type) {
                .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_WHILE, .TOKEN_IF, .TOKEN_PRINT, .TOKEN_RETURN => return,
                else => self.advance(),
            }
        }
    }

    fn function(self: *Parser, ftype: FunctionType) void {
        var compiler = Compiler.init(self.vm, ftype, self.compiler);
        self.compiler = &compiler;
        self.compiler.function.name = Obj.String.copy(self.vm, self.previous.lexeme);
        self.beginScope();

        self.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after function name.");
        if (!self.check(TokenType.TOKEN_RIGHT_PAREN)) {
            while (true) {
                self.compiler.function.arity += 1;
                if (self.compiler.function.arity > 255) {
                    self.errorAtCurrent("Can't have more than 255 parameters");
                }

                const constant = self.parseVariable("Expect parameter name.");
                self.defineVariable(constant);

                if (!self.match(TokenType.TOKEN_COMMA)) {
                    break;
                }
            }
        }
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
        self.consume(TokenType.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
        self.block();

        const func = self.endCompiler();
        // at this point self.compiler points to the outer compiler.

        self.emitUnaryOp(OpCode.OP_CLOSURE, self.makeConstant(Value.fromObj(&func.obj)));

        var i: usize = 0;
        while (i < func.upvalue_count) : (i += 1) {
            self.emitByte(if (compiler.upvalues[i].is_local) 1 else 0);
            self.emitByte(compiler.upvalues[i].index);
        }
    }

    fn number(self: *Parser, _: bool) void {
        if (std.fmt.parseFloat(f64, self.previous.lexeme)) |value| {
            self.emitConstant(Value.fromNumber(value));
        } else |e| switch (e) {
            std.fmt.ParseFloatError.InvalidCharacter => {
                std.debug.print("cound not parse number", .{});
            },
        }
    }

    fn @"or"(self: *Parser, _: bool) void {
        const elseJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        const endJump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(elseJump);
        self.emitOp(OpCode.OP_POP);

        self.parsePrecedence(Precedence.PREC_OR);
        self.patchJump(endJump);
    }

    fn @"and"(self: *Parser, _: bool) void {
        const endJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        self.emitOp(OpCode.OP_POP);
        self.parsePrecedence(Precedence.PREC_AND);

        self.patchJump(endJump);
    }

    fn string(self: *Parser, _: bool) void {
        const lexeme = self.previous.lexeme;
        const value = Obj.String.copy(self.vm, lexeme[1 .. lexeme.len - 1]);
        self.emitConstant(Value.fromObj(&value.obj));
    }

    fn variable(self: *Parser, can_assing: bool) void {
        self.namedVariable(&self.previous, can_assing);
    }

    fn literal(self: *Parser, _: bool) void {
        switch (self.previous.type) {
            TokenType.TOKEN_FALSE => self.emitOp(OpCode.OP_FALSE),
            TokenType.TOKEN_TRUE => self.emitOp(OpCode.OP_TRUE),
            TokenType.TOKEN_NIL => self.emitOp(OpCode.OP_NIL),
            else => unreachable,
        }
    }

    fn call(self: *Parser, _: bool) void {
        const arg_count = self.argumentList();
        self.emitUnaryOp(OpCode.OP_CALL, arg_count);
    }

    fn argumentList(self: *Parser) u8 {
        var arg_count: u8 = 0;
        if (!self.check(TokenType.TOKEN_RIGHT_PAREN)) {
            while (true) {
                self.expression();
                if (arg_count == 255) {
                    self.err("Can't have more than 255 arguments");
                }
                arg_count += 1;

                if (!self.match(TokenType.TOKEN_COMMA)) break;
            }
        }

        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
        return arg_count;
    }

    // assuming we consumed the initial '(', we call the expression function and expect to have the closing ')' after.
    fn grouping(self: *Parser, _: bool) void {
        self.expression();
        self.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn unary(self: *Parser, _: bool) void {
        const operatorType = self.previous.type;

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

    fn binary(self: *Parser, _: bool) void {
        const operatorType = self.previous.type;
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

    fn getRule(self: *Parser, ttoken: TokenType) ParseRule {
        _ = self;
        const rule = switch (ttoken) {
            TokenType.TOKEN_LEFT_PAREN => comptime ParseRule.init(Parser.grouping, Parser.call, Precedence.PREC_CALL),
            TokenType.TOKEN_RIGHT_PAREN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_LEFT_BRACE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_RIGHT_BRACE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_COMMA => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_DOT => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_MINUS => comptime ParseRule.init(Parser.unary, Parser.binary, Precedence.PREC_TERM),
            TokenType.TOKEN_PLUS => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_TERM),
            TokenType.TOKEN_SEMICOLON => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_SLASH => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_FACTOR),
            TokenType.TOKEN_STAR => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_FACTOR),
            TokenType.TOKEN_BANG => comptime ParseRule.init(Parser.unary, null, Precedence.PREC_NONE),
            TokenType.TOKEN_BANG_EQUAL => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_EQUALITY),
            TokenType.TOKEN_EQUAL => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EQUAL_EQUAL => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_EQUALITY),
            TokenType.TOKEN_GREATER => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_GREATER_EQUAL => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_LESS => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_LESS_EQUAL => comptime ParseRule.init(null, Parser.binary, Precedence.PREC_COMPARISON),
            TokenType.TOKEN_IDENTIFIER => comptime ParseRule.init(Parser.variable, null, Precedence.PREC_NONE),
            TokenType.TOKEN_STRING => comptime ParseRule.init(Parser.string, null, Precedence.PREC_NONE),
            TokenType.TOKEN_NUMBER => comptime ParseRule.init(Parser.number, null, Precedence.PREC_NONE),
            TokenType.TOKEN_AND => comptime ParseRule.init(null, Parser.@"and", Precedence.PREC_AND),
            TokenType.TOKEN_CLASS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_ELSE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FALSE => comptime ParseRule.init(Parser.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FOR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FUN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_IF => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_NIL => comptime ParseRule.init(Parser.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_OR => comptime ParseRule.init(null, Parser.@"or", Precedence.PREC_OR),
            TokenType.TOKEN_PRINT => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_RETURN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_SUPER => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_THIS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_TRUE => comptime ParseRule.init(Parser.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_VAR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_WHILE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_ERROR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_EOF => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
        };

        return rule;
    }

    fn namedVariable(self: *Parser, name: *Token, can_assign: bool) void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        var arg: u8 = undefined;

        if (self.resolveLocal(self.compiler, name)) |local_index| {
            arg = local_index;
            get_op = OpCode.OP_GET_LOCAL;
            set_op = OpCode.OP_SET_LOCAL;
        } else if (self.resolveUpvalue(self.compiler, name)) |local_index| {
            arg = local_index;
            get_op = OpCode.OP_GET_UPVALUE;
            set_op = OpCode.OP_SET_UPVALUE;
        } else {
            arg = self.identifierConstant(name);
            get_op = OpCode.OP_GET_GLOBAL;
            set_op = OpCode.OP_SET_GLOBAL;
        }

        // check if this is a setter or a getter
        if (can_assign and self.match(TokenType.TOKEN_EQUAL)) {
            self.expression();
            self.emitUnaryOp(set_op, arg);
        } else {
            self.emitUnaryOp(get_op, arg);
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        const prefixRule = self.getRule(self.previous.type).prefix orelse {
            self.err("Expect expression");
            return;
        };

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);

        prefixRule(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(self.getRule(self.current.type).precedence)) {
            self.advance();
            const infixRule = self.getRule(self.previous.type).infix orelse {
                self.err("Expect expression");
                return;
            };

            infixRule(self, can_assign);
        }

        if (can_assign and self.match(TokenType.TOKEN_EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) u8 {
        self.consume(TokenType.TOKEN_IDENTIFIER, errorMessage);

        self.declareVariable();
        //At runtime, locals aren’t looked up by name.
        //There’s no need to stuff the variable’s name into the constant table,
        //so if the declaration is inside a local scope, we return a dummy table index instead.
        if (self.compiler.scope_depth > 0) return 0;

        return self.identifierConstant(&self.previous);
    }

    fn defineVariable(self: *Parser, global: u8) void {
        //no need to define a local variable because value on top of the stack is the local variable.
        if (self.compiler.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        self.emitUnaryOp(OpCode.OP_DEFINE_GLOBAL, global);
    }

    /// adds the variable to the compilers list of variables in the current scope.
    fn declareVariable(self: *Parser) void {
        if (self.compiler.scope_depth == 0) return;

        const name: *Token = &self.previous;

        var i: isize = @as(isize, self.compiler.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.compiler.locals[@intCast(i)];
            if (local.depth != null and local.depth.? < self.compiler.scope_depth) {
                break;
            }

            if (identifiersEqual(name, &local.name)) {
                self.err("Already a variable with this name in this scope.");
            }
        }

        self.addLocal(name.*);
    }

    fn markInitialized(self: *Parser) void {
        const scope_depth = self.compiler.scope_depth;

        if (scope_depth == 0) return;

        var locals = &self.compiler.locals;
        locals[self.compiler.local_count - 1].depth = scope_depth;
    }

    fn identifierConstant(self: *Parser, name: *Token) u8 {
        const identifier = Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(Value.fromObj(&identifier.obj));
    }

    fn identifiersEqual(a: *Token, b: *Token) bool {
        if (a.lexeme.len != b.lexeme.len) return false;

        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    pub fn addLocal(self: *Parser, name: Token) void {
        if (self.compiler.local_count >= UNIT8_COUNT) {
            self.err("Too many local variables if function.");
            return;
        }

        const local = &self.compiler.locals[self.compiler.local_count];
        self.compiler.local_count += 1;

        local.name = name;
        local.depth = null;
        local.is_captured = false;
    }

    fn resolveLocal(self: *Parser, compiler: *Compiler, name: *Token) ?u8 {
        var i: isize = compiler.local_count - 1;

        while (i >= 0) : (i -= 1) {
            var local = compiler.locals[@intCast(i)];
            if (identifiersEqual(name, &local.name)) {
                if (local.depth) |_| {
                    return @intCast(i);
                } else {
                    self.err("Cannot read local variable in its own initializer.");
                }
            }
        }

        return null;
    }

    fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: *Token) ?u8 {
        if (compiler.enclosing == null) return null;

        if (self.resolveLocal(compiler.enclosing.?, name)) |local| {
            compiler.enclosing.?.locals[local].is_captured = true;
            return self.addUpvalue(compiler, local, true);
        }

        if (self.resolveUpvalue(compiler.enclosing.?, name)) |index| {
            return self.addUpvalue(compiler, index, false);
        }

        return null;
    }

    fn addUpvalue(self: *Parser, compiler: *Compiler, index: u8, isLocal: bool) ?u8 {
        const upvalue_count = compiler.function.upvalue_count;

        var i: usize = 0;
        while (i < upvalue_count) : (i += 1) {
            const upvalue = &compiler.upvalues[i];

            if (upvalue.index == index and upvalue.is_local == isLocal) {
                return @intCast(i);
            }
        }

        if (upvalue_count == UNIT8_COUNT) {
            self.err("Too many closure variables in function.");
            return null;
        }

        compiler.upvalues[upvalue_count].is_local = isLocal;
        compiler.upvalues[upvalue_count].index = index;

        compiler.function.upvalue_count += 1;
        return upvalue_count;
    }

    fn beginScope(self: *Parser) void {
        self.compiler.scope_depth += 1;
    }

    fn endScope(self: *Parser) void {
        self.compiler.scope_depth -= 1;

        while (self.compiler.local_count > 0 and
            self.compiler.locals[self.compiler.local_count - 1].depth.? > self.compiler.scope_depth) : (self.compiler.local_count -= 1)
        {
            if (self.compiler.locals[self.compiler.local_count - 1].is_captured) {
                self.emitOp(OpCode.OP_CLOSE_UPVALUE);
            } else {
                self.emitOp(OpCode.OP_POP);
            }
        }
    }
};

/// track the compiler state for locals - https://craftinginterpreters.com/local-variables.html#representing-local-variables
const Compiler = struct {
    enclosing: ?*Compiler,
    locals: [UNIT8_COUNT]Local = undefined,
    upvalues: [UNIT8_COUNT]Upvalue = undefined,
    local_count: u32 = 0,
    scope_depth: u32 = 0,
    function: *Obj.Function,
    function_type: FunctionType,

    const Local = struct {
        name: Token,
        /// records the scope depth of the block where the local variable was declared
        depth: ?u32,
        /// is captured by closure
        is_captured: bool,
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    pub fn init(vm: *VM, ftype: FunctionType, enclosing: ?*Compiler) Compiler {
        var compiler = Compiler{ .function = Obj.Function.create(vm), .function_type = ftype, .enclosing = enclosing };
        // First local is reserved to represent the current function
        // value on the stack. Give it a name of "" to make sure it
        // can't actually be referenced by local variables.
        var local = &compiler.locals[compiler.local_count];
        compiler.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        local.name.lexeme = "";

        return compiler;
    }
};

const ParseFn = *const fn (parser: *Parser, can_assign: bool) void;
