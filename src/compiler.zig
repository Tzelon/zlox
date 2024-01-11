const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const TokenType = Scanner.TokenType;
const Token = Scanner.Token;
const Chunk = @import("./chunk.zig").Chunk;

const errout = std.io.getStdErr().writer();

pub fn compile(source: []const u8, chunk: *Chunk) bool {
    _ = chunk;
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    parser.advance();
    // expression();
    parser.consume(TokenType.TOKEN_EOF, "Expecte end of expression.");

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
