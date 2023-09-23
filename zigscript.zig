const builtin = @import("builtin");
const std = @import("std");

const global = struct {
    var error_arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    pub const error_arena = error_arena_instance.allocator();
};


pub fn main() !void {
    try testError("@badBuiltin()", "unknown builtin '@badBuiltin'");
    try testError("@out()", "@out requires 1 argument but got 0");
    try testError("@out(0)", "@out requires argument 1 to be of type string but got number");
    try testExpr(
        \\@out("Hello, World!\n")
    );
}

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const Vm = struct {
    src: [:0]const u8,
    allocator: std.mem.Allocator,
    stack: std.ArrayListUnmanaged(Value) = .{},

    pub fn deinit(self: *Vm) void {
        for (self.stack.items) |item| {
            item.deinit(self.allocator);
        }
        self.stack.deinit(self.allocator);
    }

    pub fn push_string_literal(self: *Vm, loc: std.zig.Token.Loc) void {
        self.stack.append(self.allocator, .{
            // TODO: escape the string literal
            .string = self.allocator.dupe(u8, self.src[loc.start..loc.end]) catch |e| oom(e),
        }) catch |e| oom(e);
    }

    pub fn push_number_literal(self: *Vm, loc: std.zig.Token.Loc) void {
        self.stack.append(self.allocator, .{
            // TODO: escape the string literal
            .number = self.allocator.dupe(u8, self.src[loc.start..loc.end]) catch |e| oom(e),
        }) catch |e| oom(e);
    }

    pub fn call_builtin(self: *Vm, loc: std.zig.Token.Loc) ?[]u8 {
        const name = self.src[loc.start..loc.end];
        if (std.mem.eql(u8, name, "@out")) {
            if (self.stack.items.len != 1) return std.fmt.allocPrint(
                global.error_arena,
                "@out requires 1 argument but got {}",
                .{self.stack.items.len},
            ) catch |e| oom(e);
            if (self.stack.items[0] != .string) return std.fmt.allocPrint(
                global.error_arena,
                "@out requires argument 1 to be of type string but got {s}",
                .{self.stack.items[0].error_desc()},
            ) catch |e| oom(e);
            std.io.getStdOut().writer().writeAll(self.stack.items[0].string)
                catch |err| std.debug.panic("@out failed with {s}", .{@errorName(err)});
            self.stack.items[0].deinit(self.allocator);
            self.stack.items.len = 0;
            return null;
        } else {
            return std.fmt.allocPrint(global.error_arena, "unknown builtin '{s}'", .{name}) catch |e| oom(e);
        }
    }
};

const Value = union(enum) {
    string: []const u8,
    number: []const u8,
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |s| allocator.free(s),
            .number => |n| allocator.free(n),
        }
    }
    pub fn error_desc(self: Value) []const u8 {
        return switch (self) {
            .string => "string",
            .number => "number",
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
fn PrimaryTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ErrorOr(usize) {
    const first_token = lex(src, start);
    if (first_token.tag == .builtin) {
        if (FnCallArguments(src, first_token.loc.end, null)) |fn_call_end| {
            if (vm_opt) |vm| {
                const new_end = FnCallArguments(src, first_token.loc.end, vm);
                std.debug.assert(new_end == fn_call_end);
                if (vm.call_builtin(first_token.loc)) |msg|
                    return .{ .err = msg };
            }
            return .{ .ok = fn_call_end };
        }
    }
    std.debug.panic("todo: token tag={s} src='{s}'", .{@tagName(first_token.tag), src[first_token.loc.start..first_token.loc.end]});
}

fn FnCallArguments(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    const expr_list_start = blk: {
        const token = lex(src, start);
        if (token.tag != .l_paren) return null;
        break :blk token.loc.end;
    };

    const expr_list_end = ExprList(src, expr_list_start, null);

    const r_paren_token = lex(src, expr_list_end);
    if (r_paren_token.tag != .r_paren) {
        //std.log.info("Not FnCallArguments, token={s}", .{@tagName(r_paren_token.tag)});
        return null;
    }

    if (vm_opt) |vm| {
        const new_end = ExprList(src, expr_list_start, vm);
        std.debug.assert(new_end == expr_list_end);
    }
    return r_paren_token.loc.end;
}

// ExprList <- (Expr COMMA)* Expr?
fn ExprList(src: [:0]const u8, start: usize, vm_opt: ?*Vm) usize {
    var off = start;
    while (Expr(src, off, null)) |expr_end| {
        if (vm_opt) |vm| {
            const new_end = Expr(src, off, vm);
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
fn Expr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    const token = lex(src, start);

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .string_literal) {
        if (vm_opt) |vm| vm.push_string_literal(token.loc);
        return token.loc.end;
    }
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .number_literal) {
        if (vm_opt) |vm| vm.push_number_literal(token.loc);
        return token.loc.end;
    }
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // GRAMMAR HACK for now
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (token.tag == .r_paren) {
        return null;
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
    const expr_end = switch (PrimaryTypeExpr(src, 0, &vm)) {
        .ok => |end| end,
        .err => |error_msg| {
            defer global.error_arena.free(error_msg);
            std.log.err("src '{s}' unexpectedly failed with {s}", .{src, error_msg});
            return error.TestUnexpectedResult;
        },
    };
    if (expr_end != src.len) {
        std.log.err("src '{s}' is not a PrimaryTypeExpr (end={})", .{src, expr_end});
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
    _ = switch (PrimaryTypeExpr(src, 0, &vm)) {
        .ok => {},
        .err => |error_msg| {
            defer global.error_arena.free(error_msg);
            return std.testing.expectEqualSlices(u8, expected_error, error_msg);
        },
    };
    std.log.err("src '{s}' unexpectedly didn't have an error", .{src});
    return error.TestUnexpectedResult;
}
