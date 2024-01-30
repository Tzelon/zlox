const std = @import("std");

pub const Scanner = struct {
    start: []const u8,
    current: usize,
    line: usize,

    pub const TokenType = enum {

        // Single-character tokens.
        TOKEN_LEFT_PAREN,
        TOKEN_RIGHT_PAREN,
        TOKEN_LEFT_BRACE,
        TOKEN_RIGHT_BRACE,
        TOKEN_COMMA,
        TOKEN_DOT,
        TOKEN_MINUS,
        TOKEN_PLUS,
        TOKEN_SEMICOLON,
        TOKEN_SLASH,
        TOKEN_STAR,
        // One or two character tokens.
        TOKEN_BANG,
        TOKEN_BANG_EQUAL,
        TOKEN_EQUAL,
        TOKEN_EQUAL_EQUAL,
        TOKEN_GREATER,
        TOKEN_GREATER_EQUAL,
        TOKEN_LESS,
        TOKEN_LESS_EQUAL,
        // Literals.
        TOKEN_IDENTIFIER,
        TOKEN_STRING,
        TOKEN_NUMBER,
        // Keywords.
        TOKEN_AND,
        TOKEN_CLASS,
        TOKEN_ELSE,
        TOKEN_FALSE,
        TOKEN_FOR,
        TOKEN_FUN,
        TOKEN_IF,
        TOKEN_NIL,
        TOKEN_OR,
        TOKEN_PRINT,
        TOKEN_RETURN,
        TOKEN_SUPER,
        TOKEN_THIS,
        TOKEN_TRUE,
        TOKEN_VAR,
        TOKEN_WHILE,

        TOKEN_ERROR,
        TOKEN_EOF,
    };

    pub const Token = struct { type: TokenType, lexeme: []const u8, line: usize };

    pub fn init(source: []const u8) Scanner {
        return Scanner{ .start = source, .current = 0, .line = 1 };
    }

    pub fn scanToken(self: *Scanner) ?Token {
        self.skipWhiteSpace();
        self.start = self.start[self.current..];
        self.current = 0;

        if (self.isAtEnd()) {
            return self.makeToken(.TOKEN_EOF);
        }
        const c = self.advance();
        if (isDigit(c)) return self.number();
        if (isAlpha(c)) return self.identifier();

        return switch (c) {
            '(' => self.makeToken(.TOKEN_LEFT_PAREN),
            ')' => self.makeToken(.TOKEN_RIGHT_PAREN),
            '{' => self.makeToken(.TOKEN_LEFT_BRACE),
            '}' => self.makeToken(.TOKEN_RIGHT_BRACE),
            ';' => self.makeToken(.TOKEN_SEMICOLON),
            ',' => self.makeToken(.TOKEN_COMMA),
            '.' => self.makeToken(.TOKEN_DOT),
            '-' => self.makeToken(.TOKEN_MINUS),
            '+' => self.makeToken(.TOKEN_PLUS),
            '/' => self.makeToken(.TOKEN_SLASH),
            '*' => self.makeToken(.TOKEN_STAR),
            '!' => if (self.match('=')) self.makeToken(.TOKEN_BANG_EQUAL) else self.makeToken(.TOKEN_BANG),
            '=' => if (self.match('=')) self.makeToken(.TOKEN_EQUAL_EQUAL) else self.makeToken(.TOKEN_EQUAL),
            '<' => if (self.match('=')) self.makeToken(.TOKEN_LESS_EQUAL) else self.makeToken(.TOKEN_LESS),
            '>' => if (self.match('=')) self.makeToken(.TOKEN_GREATER_EQUAL) else self.makeToken(.TOKEN_GREATER),
            '"' => self.string(),
            else => {
                return self.errorToken("unexpected character.");
            },
        };
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }

        return self.makeToken(self.identifierType());
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        //Look for fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "."
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.TOKEN_NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }

            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        _ = self.advance();
        return self.makeToken(.TOKEN_STRING);
    }

    fn makeToken(self: *Scanner, ttype: TokenType) Token {
        return Token{ .type = ttype, .lexeme = self.start[0..self.current], .line = self.line };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{ .type = .TOKEN_ERROR, .lexeme = message, .line = self.line };
    }

    fn skipWhiteSpace(self: *Scanner) void {
        while (true) {
            const c = self.peek();

            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of line.
                        // We use peek() to check for the newline but not consume it.
                        // That way, the newline will be the current character on the next turn of the outer loop in skipWhitespace()
                        // and we’ll recognize it and increment scanner.line.
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => {
                    break;
                },
            }
        }
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.start[0]) {
            'a' => self.checkKeyword(1, "nd", .TOKEN_AND),
            'c' => self.checkKeyword(1, "lass", .TOKEN_CLASS),
            'e' => self.checkKeyword(1, "lse", .TOKEN_ELSE),
            'i' => self.checkKeyword(1, "f", .TOKEN_IF),
            'n' => self.checkKeyword(1, "il", .TOKEN_NIL),
            'o' => self.checkKeyword(1, "r", .TOKEN_OR),
            'p' => self.checkKeyword(1, "rint", .TOKEN_PRINT),
            'r' => self.checkKeyword(1, "eturn", .TOKEN_RETURN),
            's' => self.checkKeyword(1, "uper", .TOKEN_SUPER),
            'v' => self.checkKeyword(1, "ar", .TOKEN_VAR),
            'w' => self.checkKeyword(1, "hile", .TOKEN_WHILE),
            'f' => {
                return switch (self.start[1]) {
                    'a' => self.checkKeyword(2, "lse", .TOKEN_FALSE),
                    'o' => self.checkKeyword(2, "r", .TOKEN_FOR),
                    'u' => self.checkKeyword(2, "n", .TOKEN_FUN),
                    else => .TOKEN_IDENTIFIER,
                };
            },
            't' => {
                return switch (self.start[1]) {
                    'h' => self.checkKeyword(2, "is", .TOKEN_THIS),
                    'r' => self.checkKeyword(2, "ue", .TOKEN_TRUE),
                    else => .TOKEN_IDENTIFIER,
                };
            },
            else => .TOKEN_IDENTIFIER,
        };
    }

    fn checkKeyword(self: *Scanner, offset: u8, rest: []const u8, ttoken: TokenType) TokenType {
        if (self.current != rest.len + offset) {
            return .TOKEN_IDENTIFIER;
        }

        const source_slice = self.start[offset..self.current];
        std.debug.assert(source_slice.len == rest.len);

        return if (std.mem.eql(u8, source_slice, rest)) ttoken else .TOKEN_IDENTIFIER;
    }

    fn isDigit(char: u8) bool {
        return '0' <= char and char <= '9';
    }

    fn isAlpha(char: u8) bool {
        return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or char == '_';
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.start.len) return 0;

        return self.start[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.start[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    pub fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.start[self.current - 1];
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.start.len;
    }
};
