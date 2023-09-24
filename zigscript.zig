const builtin = @import("builtin");
const std = @import("std");

pub fn main() !void {
    try testError("@badBuiltin()", "unknown builtin");

    try testError("@assert()", "@assert requires 1 argument but got 0");
    try testError("@assert(0)", "@assert requires argument 1 to be of type bool but got number");
    try testError("@assert(false)", "assert failed");
    try testExpr("@assert(true)");

    try testError("@out()", "@out requires 1 argument but got 0");
    try testError("@out(0)", "@out requires argument 1 to be of type string but got number");
    try testExpr(
        \\@out("Hello, World!\n")
    );
    try testError(
        \\@out("\?")
        , "invalid string literal",
    );
}

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const TokenError = enum {
    assert_failed,
    unknown_builtin,
    invalid_string_literal,
};
const VmError = struct {
    pos: usize,
    kind: union(enum) {
        token: TokenError,
        general: []const u8,
    },

    pub fn deinit(self: VmError, allocator: std.mem.Allocator) void {
        switch (self.kind) {
            .token => {},
            .general => |msg| allocator.free(msg),
        }
    }

    pub fn getTestMsg(self: VmError) []const u8 {
        switch (self.kind) {
            .token => |token_error| switch (token_error) {
                .assert_failed => return "assert failed",
                .unknown_builtin => return "unknown builtin",
                .invalid_string_literal => return "invalid string literal",
            },
            .general => |msg| return msg,
        }
    }
};

const Vm = struct {
    src: [:0]const u8,
    allocator: std.mem.Allocator,
    stack: std.ArrayListUnmanaged(Value) = .{},
    err: ?VmError = null,

    pub fn deinit(self: *Vm) void {
        if (self.err) |err| {
            err.deinit(self.allocator);
        }
        for (self.stack.items) |item| {
            item.deinit(self.allocator);
        }
        self.stack.deinit(self.allocator);
    }

    fn tokenError(self: *Vm, token_pos: usize, token_error: TokenError) error{Vm} {
        std.debug.assert(self.err == null);
        self.err = .{ .pos = token_pos, .kind = .{ .token = token_error } };
        return error.Vm;
    }
    fn generalError(self: *Vm, pos: usize, comptime fmt: []const u8, args: anytype) error{Vm} {
        std.debug.assert(self.err == null);
        self.err = .{
            .pos = pos,
            .kind = .{
                .general = std.fmt.allocPrint(self.allocator, fmt, args) catch |e| oom(e),
            },
        };
        return error.Vm;
    }

    pub fn push_bool(self: *Vm, val: bool) void {
        self.stack.append(self.allocator, .{ .@"bool" = val }) catch |e| oom(e);
    }

    pub fn push_string_literal(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const token = self.src[loc.start..loc.end];
        const string = std.zig.string_literal.parseAlloc(self.allocator, token) catch |err| switch (err) {
            error.OutOfMemory => |e| oom(e),
            error.InvalidLiteral => return self.tokenError(loc.start, .invalid_string_literal),
        };
        self.stack.append(self.allocator, .{ .string = string }) catch |e| oom(e);
    }

    pub fn push_number_literal(self: *Vm, loc: std.zig.Token.Loc) !void {
        self.stack.append(self.allocator, .{
            // TODO: escape the string literal
            .number = self.allocator.dupe(u8, self.src[loc.start..loc.end]) catch |e| oom(e),
        }) catch |e| oom(e);
    }

    pub fn call_builtin(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const name = self.src[loc.start..loc.end];
        if (std.mem.eql(u8, name, "@out")) {
            if (self.stack.items.len != 1) return self.generalError(
                loc.start,
                "@out requires 1 argument but got {}",
                .{self.stack.items.len},
            );
            if (self.stack.items[0] != .string) return self.generalError(
                loc.start,
                "@out requires argument 1 to be of type string but got {s}",
                .{self.stack.items[0].error_desc()},
            );
            std.io.getStdOut().writer().writeAll(self.stack.items[0].string)
                catch |err| std.debug.panic("@out failed with {s}", .{@errorName(err)});
            self.stack.items[0].deinit(self.allocator);
            self.stack.items.len = 0;
        } else if (std.mem.eql(u8, name, "@assert")) {
            if (self.stack.items.len != 1) return self.generalError(
                loc.start,
                "@assert requires 1 argument but got {}",
                .{self.stack.items.len},
            );
            if (self.stack.items[0] != .@"bool") return self.generalError(
                loc.start,
                "@assert requires argument 1 to be of type bool but got {s}",
                .{self.stack.items[0].error_desc()},
            );
            if (!self.stack.items[0].@"bool")
                return self.tokenError(loc.start, .assert_failed);
            self.stack.items[0].deinit(self.allocator);
            self.stack.items.len = 0;
        } else {
            return self.tokenError(loc.start, .unknown_builtin);
        }
    }
};

const Value = union(enum) {
    @"bool": bool,
    number: []const u8,
    string: []const u8,
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .@"bool" => {},
            .number => |n| allocator.free(n),
            .string => |s| allocator.free(s),
        }
    }
    pub fn error_desc(self: Value) []const u8 {
        return switch (self) {
            .@"bool" => "bool",
            .number => "number",
            .string => "string",
        };
    }
};

fn ErrorOr(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: []u8,
    };
}

fn lex(src: [:0]const u8, off: usize) std.zig.Token {
    std.debug.assert(off <= src.len);
    var tokenizer = std.zig.Tokenizer{
        .buffer = src,
        .index = off,
        .pending_invalid_token = null,
    };
    return tokenizer.next();
}

// PrimaryTypeExpr
//     <- BUILTINIDENTIFIER FnCallArguments
//      / CHAR_LITERAL
//      / ContainerDecl
//      / DOT IDENTIFIER
//      / DOT InitList
//      / ErrorSetDecl
//      / FLOAT
//      / FnProto
//      / GroupedExpr
//      / LabeledTypeExpr
//      / IDENTIFIER
//      / IfTypeExpr
//      / INTEGER
//      / KEYWORD_comptime TypeExpr
//      / KEYWORD_error DOT IDENTIFIER
//      / KEYWORD_anyframe
//      / KEYWORD_unreachable
//      / STRINGLITERAL
//      / SwitchExpr
fn PrimaryTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !usize {
    const first_token = lex(src, start);
    if (first_token.tag == .builtin) {
        if (try FnCallArguments(src, first_token.loc.end, null)) |fn_call_end| {
            if (vm_opt) |vm| {
                const new_end = try FnCallArguments(src, first_token.loc.end, vm);
                std.debug.assert(new_end == fn_call_end);
                try vm.call_builtin(first_token.loc);
            }
            return fn_call_end;
        }
    }
    std.debug.panic("todo: token tag={s} src='{s}'", .{@tagName(first_token.tag), src[first_token.loc.start..first_token.loc.end]});
}

fn FnCallArguments(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !?usize {
    const expr_list_start = blk: {
        const token = lex(src, start);
        if (token.tag != .l_paren) return null;
        break :blk token.loc.end;
    };

    const expr_list_end = try ExprList(src, expr_list_start, null);

    const r_paren_token = lex(src, expr_list_end);
    if (r_paren_token.tag != .r_paren) {
        //std.log.info("Not FnCallArguments, token={s}", .{@tagName(r_paren_token.tag)});
        return null;
    }

    if (vm_opt) |vm| {
        const new_end = try ExprList(src, expr_list_start, vm);
        std.debug.assert(new_end == expr_list_end);
    }
    return r_paren_token.loc.end;
}

// ExprList <- (Expr COMMA)* Expr?
fn ExprList(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !usize {
    var off = start;
    while (try Expr(src, off, null)) |expr_end| {
        if (vm_opt) |vm| {
            const new_end = try Expr(src, off, vm);
            std.debug.assert(new_end == expr_end);
        }
        off = expr_end;
        const token = lex(src, off);
        if (token.tag != .comma)
            break;
        off = token.loc.end;
    }
    return off;
}

// Expr <- BoolOrExpr
fn Expr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !?usize {
    const token = lex(src, start);

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .string_literal) {
        if (vm_opt) |vm| try vm.push_string_literal(token.loc);
        return token.loc.end;
    }
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .number_literal) {
        if (vm_opt) |vm| try vm.push_number_literal(token.loc);
        return token.loc.end;
    }
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .r_paren) {
        return null;
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .identifier) {
        const slice = src[token.loc.start..token.loc.end];
        if (std.mem.eql(u8, slice, "false")) {
            if (vm_opt) |vm| vm.push_bool(false);
            return token.loc.end;
        }
        if (std.mem.eql(u8, slice, "true")) {
            if (vm_opt) |vm| vm.push_bool(true);
            return token.loc.end;
        }
    }

    std.debug.panic("todo: Expr token tag={s} src='{s}'", .{@tagName(token.tag), src[token.loc.start..token.loc.end]});
}

fn testExpr(src: [:0]const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (PrimaryTypeExpr(src, 0, &vm)) |end| {
        if (end != src.len) {
            std.log.err("src '{s}' is not a PrimaryTypeExpr (end={})", .{src, end});
        }
    } else |vm_err| switch (vm_err) {
        error.Vm => {
            const err = vm.err orelse @panic("vm reported error but has none?");
            const error_msg = err.getTestMsg();
            std.log.err("src '{s}' had unexpected error: {s}", .{src, error_msg});
            return error.TestUnexpectedResult;
        },
    }
}
fn testError(src: [:0]const u8, expected_error: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (PrimaryTypeExpr(src, 0, &vm)) |_| {
        std.log.err("src '{s}' unexpectedly didn't have an error", .{src});
        return error.TestUnexpectedResult;
    } else |vm_err| switch (vm_err) {
        error.Vm => {
            const err = vm.err orelse @panic("vm reported error but has none?");
            const actual_msg = err.getTestMsg();
            return std.testing.expectEqualSlices(u8, expected_error, actual_msg);
        },
    }
}
