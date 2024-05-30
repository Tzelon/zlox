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
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    var locals = Locals.init(&parser);
    var compiler = Compiler.init(vm, &parser, &locals, .TYPE_SCRIPT);
    parser.advance();
    while (!parser.match(.TOKEN_EOF)) {
        compiler.declaration();
    }
    parser.consume(TokenType.TOKEN_EOF, "Expect end of expression.");
    const function = compiler.deinit();
    return if (parser.hadError) CompileError.CompileError else function;
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

const Locals = struct {
    parser: *Parser,
    locals: [UNIT8_COUNT]Local = undefined,
    local_count: u32 = 0,
    scope_depth: u32 = 0,

    const Local = struct {
        name: Token,
        /// records the scope depth of the block where the local variable was declared
        depth: ?u32,
    };

    pub fn init(parser: *Parser) Locals {
        return Locals{ .parser = parser, .local_count = 0, .scope_depth = 0 };
    }

    pub fn add(self: *Locals, name: Token) void {
        if (self.local_count >= UNIT8_COUNT) {
            self.parser.err("Too many local variables if function.");
            return;
        }

        const local = &self.locals[self.local_count];
        self.local_count += 1;

        local.name = name;
        local.depth = null;
    }

    pub fn markInitialized(self: *Locals) void {
        if (self.scope_depth == 0) return;
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }
};

const ParseFn = *const fn (compiler: *Compiler, can_assign: bool) void;

const Compiler = struct {
    parser: *Parser,
    locals: *Locals,
    function: *Obj.Function,
    type: FunctionType,
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

    pub fn init(vm: *VM, parser: *Parser, locals: *Locals, ftype: FunctionType) Compiler {
        // First local is reserved to represent the current function
        // value on the stack. Give it a name of "" to make sure it
        // can't actually be referenced by local variables.
        const local = &locals.locals[locals.local_count];
        locals.local_count += 1;
        local.depth = 0;
        local.name.lexeme = "";

        return Compiler{ .vm = vm, .parser = parser, .locals = locals, .function = Obj.Function.create(vm), .type = ftype };
    }

    pub fn deinit(self: *Compiler) *Obj.Function {
        self.emitReturn();
        const function = self.function;

        if (debug.debug_print_code and !self.parser.hadError) {
            debug.disassembleChunk(self.currentChunk(), if (function.name) |name| name.chars else "<script>");
        }

        return function;
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

    pub fn emitLoop(self: *Compiler, loopStart: usize) void {
        self.emitOp(OpCode.OP_LOOP);

        const offset = self.currentChunk().code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.parser.err("Loop body too large.");
        }

        self.emitByte(@intCast((offset >> 8) & 0xff));
        self.emitByte(@intCast(offset & 0xff));
    }

    pub fn emitJump(self: *Compiler, instruction: OpCode) usize {
        self.emitOp(instruction);
        // dummy operands that will be patched later.
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn emitUnaryOp(self: *Compiler, opcode: OpCode, byte: u8) void {
        self.emitOp(opcode);
        self.emitByte(byte);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitUnaryOp(OpCode.OP_CONSTANT, self.makeConstant(value));
    }

    fn patchJump(self: *Compiler, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.parser.err("Too much code to jump over.");
        }

        self.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emitReturn(self: *Compiler) void {
        self.emitOp(OpCode.OP_RETURN);
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return &self.function.chunk;
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
        if (self.parser.match(TokenType.TOKEN_FUN)) {
            self.funDeclaration();
        } else if (self.parser.match(TokenType.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.parser.panicMode) self.synchronize();
    }

    fn statement(self: *Compiler) void {
        if (self.parser.match(TokenType.TOKEN_PRINT)) {
            self.printStatement();
        } else if (self.parser.match(TokenType.TOKEN_IF)) {
            self.ifStatement();
        } else if (self.parser.match(TokenType.TOKEN_WHILE)) {
            self.whileStatement();
        } else if (self.parser.match(TokenType.TOKEN_FOR)) {
            self.forStatement();
        } else if (self.parser.match(TokenType.TOKEN_LEFT_BRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn block(self: *Compiler) void {
        while (!self.parser.check(TokenType.TOKEN_RIGHT_BRACE) and !self.parser.check(TokenType.TOKEN_EOF)) {
            self.declaration();
        }

        self.parser.consume(TokenType.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn funDeclaration(self: *Compiler) void {
        const global = self.parseVariable("Expect function name");
        self.locals.markInitialized();
        function(.TYPE_FUNCTION);
        self.defineVariable(global);
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

    fn ifStatement(self: *Compiler) void {
        self.parser.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const thenJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        self.emitOp(OpCode.OP_POP);
        self.statement();

        const elseJump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(thenJump);
        self.emitOp(OpCode.OP_POP);

        if (self.parser.match(TokenType.TOKEN_ELSE)) {
            self.statement();
        }

        self.patchJump(elseJump);
    }

    fn forStatement(self: *Compiler) void {
        self.beginScope();
        self.parser.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        //initializer
        if (self.parser.match(TokenType.TOKEN_SEMICOLON)) {
            // no initializer
        } else if (self.parser.match(TokenType.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.items.len;
        var exitJump: usize = undefined;

        // condition
        if (!self.parser.match(TokenType.TOKEN_SEMICOLON)) {
            self.expression();
            self.parser.consume(TokenType.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

            // jump out of the loop if the condition is false.
            exitJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
            self.emitOp(OpCode.OP_POP);
        }

        // increment
        if (!self.parser.match(TokenType.TOKEN_RIGHT_PAREN)) {
            const bodyJump = self.emitJump(OpCode.OP_JUMP);
            const incrementStart = self.currentChunk().code.items.len;
            self.expression();
            self.emitOp(OpCode.OP_POP);

            self.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

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

    fn whileStatement(self: *Compiler) void {
        const loopStart = self.currentChunk().code.items.len;
        self.parser.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exitJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        self.emitOp(OpCode.OP_POP);
        self.statement();
        self.emitLoop(loopStart);

        self.patchJump(exitJump);
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

    fn function(self: *Compiler, ftype: FunctionType) void {
        const compiler = Compiler.init(self.vm, self.parser, self.locals, ftype);
        compiler.beginScope();

        compiler.parser.consume(TokenType.TOKEN_LEFT_PAREN, "Expect '(' after function name.");
        compiler.parser.consume(TokenType.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
        compiler.parser.consume(TokenType.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
        compiler.block();

        const func = compiler.deinit();

        self.emitConstant(self.makeConstant(Value.fromObj(func.obj)));
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

    fn @"or"(self: *Compiler, _: bool) void {
        const elseJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        const endJump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(elseJump);
        self.emitOp(OpCode.OP_POP);

        self.parsePrecedence(Precedence.PREC_OR);
        self.patchJump(endJump);
    }

    fn @"and"(self: *Compiler, _: bool) void {
        const endJump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        self.emitOp(OpCode.OP_POP);
        self.parsePrecedence(Precedence.PREC_AND);

        self.patchJump(endJump);
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
            TokenType.TOKEN_AND => comptime ParseRule.init(null, Compiler.@"and", Precedence.PREC_AND),
            TokenType.TOKEN_CLASS => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_ELSE => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FALSE => comptime ParseRule.init(Compiler.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FOR => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_FUN => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_IF => comptime ParseRule.init(null, null, Precedence.PREC_NONE),
            TokenType.TOKEN_NIL => comptime ParseRule.init(Compiler.literal, null, Precedence.PREC_NONE),
            TokenType.TOKEN_OR => comptime ParseRule.init(null, Compiler.@"or", Precedence.PREC_OR),
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
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        var arg: u8 = undefined;

        if (self.resolveLocal(name)) |local_index| {
            arg = local_index;
            get_op = OpCode.OP_GET_LOCAL;
            set_op = OpCode.OP_SET_LOCAL;
        } else {
            arg = self.identifierConstant(name);
            get_op = OpCode.OP_GET_GLOBAL;
            set_op = OpCode.OP_SET_GLOBAL;
        }

        // check if this is a setter or a getter
        if (can_assign and self.parser.match(TokenType.TOKEN_EQUAL)) {
            self.expression();
            self.emitUnaryOp(set_op, arg);
        } else {
            self.emitUnaryOp(get_op, arg);
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

        self.declareVariable();
        //At runtime, locals aren’t looked up by name.
        //There’s no need to stuff the variable’s name into the constant table,
        //so if the declaration is inside a local scope, we return a dummy table index instead.
        if (self.locals.scope_depth > 0) return 0;

        return self.identifierConstant(&self.parser.previous);
    }

    fn defineVariable(self: *Compiler, global: u8) void {
        //no need to define a local variable because value on top of the stack is the local variable.
        if (self.locals.scope_depth > 0) {
            self.locals.markInitialized();
            return;
        }

        self.emitUnaryOp(OpCode.OP_DEFINE_GLOBAL, global);
    }

    /// adds the variable to the compilers list of variables in the current scope.
    fn declareVariable(self: *Compiler) void {
        if (self.locals.scope_depth == 0) return;

        const name: *Token = &self.parser.previous;

        var i: isize = @as(isize, self.locals.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals.locals[@intCast(i)];
            if (local.depth.? != -1 and local.depth.? < self.locals.scope_depth) {
                break;
            }

            if (identifiersEqual(name, &local.name)) {
                self.parser.err("Already a variable with this name in this scope.");
            }
        }

        self.locals.add(name.*);
    }

    fn identifierConstant(self: *Compiler, name: *Token) u8 {
        const identifier = Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(Value.fromObj(&identifier.obj));
    }

    fn identifiersEqual(a: *Token, b: *Token) bool {
        if (a.lexeme.len != b.lexeme.len) return false;

        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn resolveLocal(self: *Compiler, name: *Token) ?u8 {
        var i: isize = self.locals.local_count - 1;

        while (i >= 0) : (i -= 1) {
            var local = self.locals.locals[@intCast(i)];
            if (identifiersEqual(name, &local.name)) {
                if (local.depth) |_| {
                    return @intCast(i);
                } else {
                    self.parser.err("Cannot read local variable in its own initializer.");
                }
            }
        }

        return null;
    }

    fn beginScope(self: *Compiler) void {
        self.locals.scope_depth += 1;
    }

    fn endScope(self: *Compiler) void {
        self.locals.scope_depth -= 1;

        while (self.locals.local_count > 0 and
            self.locals.locals[self.locals.local_count - 1].depth.? > self.locals.scope_depth) : (self.locals.local_count -= 1)
        {
            self.emitOp(OpCode.OP_POP);
        }
    }
};
