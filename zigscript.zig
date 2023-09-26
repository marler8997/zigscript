const builtin = @import("builtin");
const std = @import("std");

pub fn main() !void {
    try testError("@badBuiltin()", "unknown builtin");

    try testError("@assert()", "expected 1 argument(s), found 0");
    try testError("@assert(0, 0)", "expected 1 argument(s), found 2");
    try testError("@assert(0)", "expected type 'bool', found 'number'");
    try testError("@assert(false)", "assert failed");
    try testExpr("@assert(true)");

    try testError("@assert(0 or false)", "expected type 'bool', found 'number'");
    try testError("@assert(false or 0)", "expected type 'bool', found 'number'");
    try testError(
        \\@assert("hello" or "world")
        , "expected type 'bool', found 'string'"
    );
    try testExpr("@assert(true or false)");
    try testExpr("@assert(false or false or true)");
    try testError("@assert(false or false or false)", "assert failed");

    try testExpr("@assert(true and true)");
    try testError("@assert(false and true)", "assert failed");
    try testError("@assert(true and false)", "assert failed");
    try testError("@assert(false and false)", "assert failed");
    try testExpr("@assert(false and false or true)");
    try testExpr("@assert(true or false and false)");
    try testError("@assert(0 and true)", "expected type 'bool', found 'number'");
    try testError("@assert(true and 0)", "expected type 'bool', found 'number'");
    try testError(
        \\@assert("hello" and "world")
        , "expected type 'bool', found 'string'",
    );
    try testExpr("@assert(false == false)");
    try testExpr("@assert(false != true)");
    try testError("@assert(false == 0)", "incompatible types: 'bool' and 'number'");
    try testError("@assert(0 == false)", "incompatible types: 'number' and 'bool'");
    try testError("@assert(false < true)", "operator < not allowed for type 'bool'");
    try testError("@assert(false > true)", "operator > not allowed for type 'bool'");
    try testError("@assert(false <= true)", "operator <= not allowed for type 'bool'");
    try testError("@assert(false >= true)", "operator >= not allowed for type 'bool'");

    try testError("@out()", "expected 1 argument(s), found 0");
    try testError("@out(0)", "expected type 'string', found 'number'");
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
        self.stack.append(self.allocator, .{ .bool = val }) catch |e| oom(e);
    }

    pub fn push_string_literal(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const token = self.src[loc.start..loc.end];
        const string = std.zig.string_literal.parseAlloc(self.allocator, token) catch |err| switch (err) {
            error.OutOfMemory => |e| oom(e),
            error.InvalidLiteral => return self.tokenError(loc.start, .invalid_string_literal),
        };
        self.stack.append(self.allocator, .{ .string = string }) catch |e| oom(e);
    }

    pub fn push_number_literal(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        self.stack.append(self.allocator, .{
            // TODO: escape the string literal
            .number = self.allocator.dupe(u8, self.src[loc.start..loc.end]) catch |e| oom(e),
        }) catch |e| oom(e);
    }

    fn enforceArgCount(self: *Vm, loc: usize, count: usize) error{Vm}!void {
        if (self.stack.items.len != count) return self.generalError(
            loc,
            "expected {} argument(s), found {}",
            .{count, self.stack.items.len},
        );
    }

    fn popArg(self: *Vm, loc: usize, value_type: ValueType) error{Vm}!Value {
        std.debug.assert(self.stack.items.len > 0);
        const actual_type: ValueType = self.stack.items[self.stack.items.len - 1] ;
        if (actual_type != value_type) return self.generalError(
            loc,
            "expected type '{s}', found '{s}'",
            .{ value_type.error_desc(), actual_type.error_desc() },
        );
        return self.stack.pop();
    }

    pub fn callBuiltin(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const name = self.src[loc.start..loc.end];
        if (std.mem.eql(u8, name, "@out")) {
            try self.enforceArgCount(loc.start, 1);
            const msg = try self.popArg(loc.start, .string);
            defer msg.deinit(self.allocator);
            std.io.getStdOut().writer().writeAll(msg.string)
                catch |err| std.debug.panic("@out failed with {s}", .{@errorName(err)});
        } else if (std.mem.eql(u8, name, "@assert")) {
            try self.enforceArgCount(loc.start, 1);
            const value = try self.popArg(loc.start, .bool);
            if (!value.bool)
                return self.tokenError(loc.start, .assert_failed);
        } else {
            return self.tokenError(loc.start, .unknown_builtin);
        }
    }

    const BinaryBoolOp = enum {
        @"or",
        @"and",
    };

    pub fn binaryBoolOp(self: *Vm, op: BinaryBoolOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const first = try self.popArg(op_loc, .bool);
        const second = try self.popArg(op_loc, .bool);
        // we know we have capacity because we just popped off the old values
        self.stack.appendAssumeCapacity(.{
            .bool = switch (op) {
                .@"or" => first.bool or second.bool,
                .@"and" => first.bool and second.bool,
            },
        });
    }
    pub fn binaryCompareOp(self: *Vm, op: CompareOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const rhs = self.stack.pop();
        defer rhs.deinit(self.allocator);
        const lhs = self.stack.pop();
        defer lhs.deinit(self.allocator);
        // we know we have capacity because we just popped off the old values
        self.stack.appendAssumeCapacity(.{
            .bool = switch (lhs) {
                .bool => |v| try self.compareBool(op, op_loc, v, rhs),
                .number => |v| try self.compareNumber(op, op_loc, v, rhs),
                else => @panic("todo"),
            }
        });
    }

    pub fn compareBool(self: *Vm, op: CompareOp, op_loc: usize, lhs: bool, rhs_val: Value) error{Vm}!bool {
        const rhs = switch (rhs_val) {
            .bool => |rhs| rhs,
            else => |rhs_type| return self.generalError(
                op_loc, "incompatible types: 'bool' and '{s}'", .{rhs_type.error_desc()},
            ),
        };
        return switch (op) {
            .equal => lhs == rhs,
            .not_equal => lhs != rhs,
            else => self.generalError(
                op_loc, "operator {s} not allowed for type 'bool'", .{op.str()},
            ),
        };
    }

    pub fn compareNumber(self: *Vm, op: CompareOp, op_loc: usize, lhs: []const u8, rhs_val: Value) error{Vm}!bool {
        const rhs = switch (rhs_val) {
            //.bool => |rhs| rhs,
            else => |rhs_type| return self.generalError(
                op_loc, "incompatible types: 'number' and '{s}'", .{rhs_type.error_desc()},
            ),
        };
        _ = lhs;
        _ = rhs;
        return switch (op) {
            //.equal_equal => lhs == rhs,
            //.bang_equal => lhs != rhs,
            else => @panic("here"),
        };
    }
};

const ValueType = enum {
    bool,
    number,
    string,
    pub fn error_desc(self: ValueType) []const u8 {
        return switch (self) {
            .bool => "bool",
            .number => "number",
            .string => "string",
        };
    }
};
const Value = union(ValueType) {
    bool: bool,
    number: []const u8,
    string: []const u8,
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .bool => {},
            .number => |n| allocator.free(n),
            .string => |s| allocator.free(s),
        }
    }
    pub fn error_desc(self: Value) []const u8 {
        return @as(ValueType, self).error_desc();
    }
};

fn ErrorOr(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: []u8,
    };
}

const CompareOp = enum {
    equal,
    not_equal,
    less_than,
    greater_than,
    less_than_equal,
    greater_than_equal,
    pub fn str(self: CompareOp) []const u8 {
        return switch (self) {
            .equal => "==",
            .not_equal => "!=",
            .less_than => "<",
            .greater_than => ">",
            .less_than_equal => "<=",
            .greater_than_equal => ">=",
        };
    }
};
fn asCompareOp(token: std.zig.Token) ?CompareOp {
    return switch (token.tag) {
        .equal_equal => .equal,
        .bang_equal => .not_equal,
        .angle_bracket_left => .less_than,
        .angle_bracket_right => .greater_than,
        .angle_bracket_left_equal => .less_than_equal,
        .angle_bracket_right_equal => .greater_than_equal,
        else => return null,
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
                try vm.callBuiltin(first_token.loc);
            }
            return fn_call_end;
        }
    }
    std.debug.panic("todo: token tag={s} src='{s}'", .{@tagName(first_token.tag), src[first_token.loc.start..first_token.loc.end]});
}

// FnCallArguments <- LPAREN ExprList RPAREN
fn FnCallArguments(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !?usize {
    const expr_list_start = blk: {
        const token = lex(src, start);
        if (token.tag != .l_paren) return null;
        break :blk token.loc.end;
    };

    const expr_list_end = try ExprList(src, expr_list_start, null);

    const r_paren_token = lex(src, expr_list_end);
    if (r_paren_token.tag != .r_paren) {
        std.log.info("Not FnCallArguments, token={s}", .{@tagName(r_paren_token.tag)});
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
const Expr = BoolOrExpr;

// BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*
fn BoolOrExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {

    const first_expr_end = try BoolAndExpr(src, start, null) orelse return null;
    if (vm_opt) |vm|
        std.debug.assert(first_expr_end == try BoolAndExpr(src, start, vm));

    var off = first_expr_end;
    while (true) {
        const token = lex(src, off);
        if (token.tag != .keyword_or) break;

        const expr_end = try BoolAndExpr(src, token.loc.end, null) orelse break;
        if (vm_opt) |vm| {
            std.debug.assert(expr_end == try BoolAndExpr(src, token.loc.end, vm));
            try vm.binaryBoolOp(.@"or", token.loc.start);
        }

        off = expr_end;
    }
    return off;
}

// BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*
fn BoolAndExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {

    const first_expr_end = try CompareExpr(src, start, null) orelse return null;
    if (vm_opt) |vm|
        std.debug.assert(first_expr_end == try CompareExpr(src, start, vm));

    var off = first_expr_end;
    while (true) {
        const token = lex(src, off);
        if (token.tag != .keyword_and) break;

        const expr_end = try CompareExpr(src, token.loc.end, null) orelse break;
        if (vm_opt) |vm| {
            std.debug.assert(expr_end == try CompareExpr(src, token.loc.end, vm));
            try vm.binaryBoolOp(.@"and", token.loc.start);
        }

        off = expr_end;
    }
    return off;
}

// CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?
fn CompareExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const first_expr_end = try BitwiseExpr(src, start, null) orelse return null;
    if (vm_opt) |vm|
        std.debug.assert(first_expr_end == try BitwiseExpr(src, start, vm));

    var off = first_expr_end;
    while (true) {
        const token = lex(src, off);
        const compare_op = asCompareOp(token) orelse break;

        const expr_end = try BitwiseExpr(src, token.loc.end, null) orelse break;
        if (vm_opt) |vm| {
            std.debug.assert(expr_end == try BitwiseExpr(src, token.loc.end, vm));
            try vm.binaryCompareOp(compare_op, token.loc.start);
        }

        off = expr_end;
    }
    return off;
}

// BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*
fn BitwiseExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
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
