const builtin = @import("builtin");
const std = @import("std");
const interp = @import("interp.zig");

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

    try testExpr("@assert(0 == 0)");
    try testExpr("@assert(9 == 9)");
    try testExpr("@assert(9 == 0x9)");
    try testExpr("@assert(7 == 0o7)");
    try testExpr("@assert(10 == 0xa)");
    try testExpr("@assert(0b1 == 1)");
    try testExpr("@assert(0b1010_0110 == 0xa6)");
    try testError(
        \\@assert(0 == "Hello")
        , "cannot compare strings with ==",
    );
    try testError("@assert(0x0)", "expected type 'bool', found 'number'");
    try testError("@assert(0o0)", "expected type 'bool', found 'number'");
    try testError("@assert(0b0)", "expected type 'bool', found 'number'");
    try testError("@assert(1_000_000)", "expected type 'bool', found 'number'");
    try testExpr("@assert(0 != 1)");
    try testError("@assert(0 != 0)", "assert failed");
    try testExpr("@assert(0 < 1)");
    try testError("@assert(0 > 1)", "assert failed");
    try testExpr("@assert(1 > 0)");
    try testError("@assert(0 > 1)", "assert failed");
    try testExpr("@assert(0 <= 1)");
    try testError("@assert(0 >= 1)", "assert failed");
    try testExpr("@assert(1 >= 0)");
    try testError("@assert(0 >= 1)", "assert failed");

    try testError(
        \\@assert("a" == "a")
        , "cannot compare strings with =="
    );
    try testError(
        \\@assert("a" != "a")
        , "cannot compare strings with !="
    );
    try testError(
        \\@assert("a" < "a")
        , "cannot compare strings with <"
    );
    try testError(
        \\@assert("a" <= "a")
        , "cannot compare strings with <="
    );
    try testError(
        \\@assert("a" > "a")
        , "cannot compare strings with >"
    );
    try testError(
        \\@assert("a" >= "a")
        , "cannot compare strings with >="
    );

    try testError("@out()", "expected 1 argument(s), found 0");
    try testError("@out(0)", "expected type 'string', found 'number'");
    try testExpr(
        \\@out("Hello, World!\n")
    );
    try testError(
        \\@out("\?")
        , "invalid string literal",
    );

    try testError("@assert(false ++ false)", "expected indexable; found 'bool'");
    try testError("@assert(0 ++ false)", "expected indexable; found 'number'");
    try testError("@assert(\"a\" ++ false)", "expected indexable; found 'bool'");
    try testError("@assert(\"a\" ++ 0)", "expected indexable; found 'number'");
    try testExpr(
        \\@out("Hello, " ++ "World with Concat!\n")
    );
    try testError("@assert(false + false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false - false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false +% false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false -% false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false +| false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false -| false)", "invalid operands to binary expression 'bool' and 'bool'");
    try testError("@assert(false + \"a\")", "invalid operands to binary expression 'bool' and 'string'");
    try testError("@assert(\"a\" + true)", "invalid operands to binary expression 'string' and 'bool'");
    try testError("@assert(\"a\" + \"a\")", "invalid operands to binary expression 'string' and 'string'");

    try testExpr("@assert(0 + 0 == 0)");
    try testExpr("@assert(12 + 34 == 46)");
    try testExpr("@assert(168 == 78 + 90)");
    try testError("@assert(169 == 78 + 90)", "assert failed");
    try testExpr("@assert(3 - 1 == 2)");
    try testExpr("@assert(14 == 21 - 7)");
    try testExpr("@assert(1 - 1 == 0)");
    try testExpr("@assert(0 - 0 == 0)");
    try testError("@assert(0 +% 0)", "zigscript doesn't support the +% operator");
    try testError("@assert(0 -% 0)", "zigscript doesn't support the -% operator");
    try testError("@assert(0 +| 0)", "zigscript doesn't support the +| operator");
    try testError("@assert(0 -| 0)", "zigscript doesn't support the -| operator");

    try testError("@assert(0 || 0)", "not implemented");
    try testError("@assert(false * false)", "incompatible types: 'bool' and 'bool'");
    try testError("@assert(\"a\" * \"b\")", "incompatible types: 'string' and 'string'");
    try testExpr("@assert(0 * 0 == 0)");
    try testExpr("@assert(0 * 9 == 0)");
    try testExpr("@assert(99 * 0 == 0)");
    try testExpr("@assert(1 * 1 == 1)");
    try testExpr("@assert(1 * 123 == 123)");
    try testExpr("@assert(89 * 76 == 6764)");

    try testExpr("@assert(!false)");
    try testError("@assert(!true)", "assert failed");
    try testExpr("@assert(!!true)");
    try testError("@assert(!!!true)", "assert failed");
    try testError("@assert(!0)", "expected type 'bool' found 'number'");
    try testError("@assert(!\"a\")", "expected type 'bool' found 'string'");

    try testError("@assert(-false)", "negation of type 'bool'");
    try testError("@assert(-\"a\")", "negation of type 'string'");
    try testExpr("@assert(0 - 1 == -1)");
    try testExpr("@assert(-123 == -100 - 23)");

    try testError("@assert(?0)", "not implemented");
    try testError("@assert(??0)", "not implemented");
    try testError("@assert(0!0)", "not implemented");

    try testError("'a'", "not implemented");
    try testError(".a", "not implemented");

    try testExpr("@assert((true))");
    try testExpr("@assert(((true)))");
    try testError("a", "undeclared identifier");
    try testExpr("false");
    try testExpr("true");
    try testExpr("0");
    try testExpr("\"hello\"");

    try testExpr("comptime false");
    try testExpr("comptime 0");
    try testExpr("comptime \"hello\"");

    try testSrc("//! a doc comment");
    try testExpr("fn foo()");
    try testExpr("fn foo(bar)");
    try testExpr("fn foo(bar,)");
    try testExpr("fn foo(bar,baz)");
    try testExpr("fn foo(bar,baz,)");

    try testSrc("fn foo();");
    try testSrc("fn foo(){ }");

    try testBlock("{}");
    try testBlock("{@assert(true);}");
    try testBlockError("{@assert(true);@assert(false);}", "assert failed");
    try testBlock("{const a = 0;}");
    try testBlock("{const a = \"hello\";}");
    try testError("@assert(a == 0)", "undeclared identifier");
    try testBlockError("{const a = 0;const a = 0;}", "redeclaration");
    try testBlock("{const a = 0;@assert(a == 0);}");
}

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const TokenError = enum {
    undeclared_identifier,
    redeclaration,
    expected_eof,
    not_implemented,
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
                .undeclared_identifier => return "undeclared identifier",
                .redeclaration => return "redeclaration",
                .expected_eof => return "expected eof",
                .not_implemented => return "not implemented",
                .assert_failed => return "assert failed",
                .unknown_builtin => return "unknown builtin",
                .invalid_string_literal => return "invalid string literal",
            },
            .general => |msg| return msg,
        }
    }
};

const Scope = struct {
    map: std.StringHashMapUnmanaged(Value) = .{},
    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        var it = self.map.iterator();
        while (it.next()) |pair| {
            pair.value_ptr.deinit(allocator);
        }
        self.map.deinit(allocator);
    }
};

pub const Vm = struct {
    src: [:0]const u8,
    allocator: std.mem.Allocator,
    stack: std.ArrayListUnmanaged(Value) = .{},
    err: ?VmError = null,
    current_function: ?Function = null,
    scope_stack: std.ArrayListUnmanaged(Scope) = .{},

    const Function = struct {
        id: ?std.zig.Token.Loc,
        params: std.ArrayListUnmanaged(std.zig.Token.Loc) = .{},
        params_done: bool = false,
    };

    pub fn deinit(self: *Vm) void {
        if (self.err) |err| {
            err.deinit(self.allocator);
        }
        for (self.stack.items) |item| {
            item.deinit(self.allocator);
        }
        self.stack.deinit(self.allocator);
        if (self.current_function) |*f| {
            f.params.deinit(self.allocator);
        }
        for (self.scope_stack.items) |*item| {
            item.deinit(self.allocator);
        }
        self.scope_stack.deinit(self.allocator);
    }

    pub fn tokenError(self: *Vm, token_pos: usize, token_error: TokenError) error{Vm} {
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

    pub fn blockStart(self: *Vm) void {
        self.scope_stack.append(self.allocator, .{}) catch |e| oom(e);
    }
    pub fn blockEnd(self: *Vm) void {
        if (self.scope_stack.items.len == 0) @panic("codebug");
        var scope = self.scope_stack.pop();
        scope.deinit(self.allocator);
    }

    pub fn pushBlockLabel(self: *Vm, loc: std.zig.Token.Loc) void {
        _ = self;
        _ = loc;
        @panic("todo");
    }

    pub fn popBlockLabel(self: *Vm, loc: std.zig.Token.Loc) void {
        _ = self;
        _ = loc;
        @panic("todo");
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
        var num = std.math.big.int.Managed.init(self.allocator) catch |e| oom(e);
        errdefer num.deinit();

        const literal = self.src[loc.start..loc.end];
        const args: struct { base: u8, str: []const u8 } = blk: {
            if (std.mem.startsWith(u8, literal, "0x"))
                break :blk .{ .base = 16, .str = literal[2..] };
            if (std.mem.startsWith(u8, literal, "0o"))
                break :blk .{ .base = 8, .str = literal[2..] };
            if (std.mem.startsWith(u8, literal, "0b"))
                break :blk .{ .base = 2, .str = literal[2..] };
            break :blk .{ .base = 10, .str = literal };
        };
        num.setString(args.base, args.str) catch |err| switch (err) {
            error.OutOfMemory => |e| oom(e),
            error.InvalidBase, error.InvalidCharacter => |e| return self.generalError(
                loc.start, "unable to convert integer literal '{s}' to bigint with {s}", .{literal, @errorName(e)}
            ),
        };

        self.stack.append(self.allocator, .{
            .number = num.toMutable(),
        }) catch |e| oom(e);
    }

    pub fn pushCharLiteral(self: *Vm, token: std.zig.Token) error{Vm}!void {
        return self.tokenError(token.loc.start, .not_implemented);
    }

    pub fn pushDotIdentifier(self: *Vm, token: std.zig.Token) error{Vm}!void {
        return self.tokenError(token.loc.start, .not_implemented);
    }

    fn scopeLookup(self: *Vm, slice: []const u8) ?*const Value {
        var i: usize = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.scope_stack.items[i];
            if (scope.map.get(slice)) |*value_ptr|
                return value_ptr;
        }
        return null;
    }

    pub fn declareAndAssignStackTop(self: *Vm, id_loc: std.zig.Token.Loc) error{Vm}!void {
        const slice = self.src[id_loc.start .. id_loc.end];
        if (self.scopeLookup(slice)) |_| return self.tokenError(id_loc.start, .redeclaration);
        if (self.scope_stack.items.len == 0) @panic("todo: assign var in global scope");
        const scope = &self.scope_stack.items[self.scope_stack.items.len-1];
        const value = self.stack.popOrNull() orelse @panic("codebug");
        scope.map.putNoClobber(self.allocator, slice, value) catch |e| oom(e);
    }

    pub fn pushID(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const slice = self.src[loc.start..loc.end];
        const value = self.scopeLookup(slice) orelse return self.tokenError(loc.start, .undeclared_identifier);
        self.stack.append(self.allocator, value.clone(self.allocator)) catch |e| oom(e);
    }

    pub fn functionProtoStart(self: *Vm, id: ?std.zig.Token.Loc) void {
        if (self.current_function) |_|
            @panic("function proto inside function proto not implemented");
        self.current_function = .{ .id = id };
    }
    pub fn functionProtoEndParams(self: *Vm) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (f.params_done) @panic("codebug");
        f.params_done = true;
    }
    pub fn functionProtoParam(self: *Vm, id: std.zig.Token.Loc) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (f.params_done) @panic("codebug");
        f.params.append(self.allocator, id) catch |e| oom(e);
    }

    pub fn functionProtoNoBody(self: *Vm) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (!f.params_done) @panic("codebug");
        self.stack.append(self.allocator, .{ .function = .{
            .id = f.id,
            .params = f.params.toOwnedSlice(self.allocator) catch |e| oom(e),
            .body = false,
        }}) catch |e| oom(e);
        self.current_function = null;
    }
    pub fn functionProtoBodyStart(self: *Vm) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (!f.params_done) @panic("codebug");
        self.stack.append(self.allocator, .{ .function = .{
            .id = f.id,
            .params = f.params.toOwnedSlice(self.allocator) catch |e| oom(e),
            .body = true,
        }}) catch |e| oom(e);
        self.current_function = null;
    }
    pub fn functionProtoBodyFailedParse(self: *Vm) void {
        if (self.stack.items.len == 0) @panic("codebug");
        const f = self.stack.pop();
        std.debug.assert(f == .function);
    }
    pub fn functionProtoBodyEnd(self: *Vm) void {
        // TODO: set the last block parsed to the function
        _ = self;
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
    pub fn binaryCompareOp(self: *Vm, op: std.math.CompareOperator, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const rhs = self.stack.pop();
        defer rhs.deinit(self.allocator);
        const lhs = self.stack.pop();
        defer lhs.deinit(self.allocator);

        if (lhs == .string or rhs == .string)
            return self.generalError(op_loc, "cannot compare strings with {s}", .{compareOpStr(op)});

        // we know we have capacity because we just popped off the old values
        self.stack.appendAssumeCapacity(.{
            .bool = switch (lhs) {
                .bool => |v| try self.compareBool(op, op_loc, v, rhs),
                .number => |v| try self.compareNumber(op, op_loc, v, rhs),
                .string => unreachable,
                .function => @panic("todo"),
            }
        });
    }

    pub fn compareBool(self: *Vm, op: std.math.CompareOperator, op_loc: usize, lhs: bool, rhs_val: Value) error{Vm}!bool {
        const rhs = switch (rhs_val) {
            .bool => |rhs| rhs,
            .string => unreachable,
            else => |rhs_type| return self.generalError(
                op_loc, "incompatible types: 'bool' and '{s}'", .{rhs_type.error_desc()},
            ),
        };
        return switch (op) {
            .eq => lhs == rhs,
            .neq => lhs != rhs,
            else => self.generalError(
                op_loc, "operator {s} not allowed for type 'bool'", .{compareOpStr(op)},
            ),
        };
    }

    pub fn compareNumber(self: *Vm, op: std.math.CompareOperator, op_loc: usize, lhs: std.math.big.int.Mutable, rhs_val: Value) error{Vm}!bool {
        const rhs = switch (rhs_val) {
            .number => |rhs| rhs,
            .string => unreachable,
            else => |rhs_type| return self.generalError(
                op_loc, "incompatible types: 'number' and '{s}'", .{rhs_type.error_desc()},
            ),
        };
        // workaround bug in std.math.big.int treating positive/negative 0 as different
        switch (op) {
            .lt, .gt, .neq => {},
            .lte, .gte, .eq => {
                if (lhs.eqlZero() and rhs.eqlZero()) return true;
            },
        }
        return lhs.toConst().order(rhs.toConst()).compare(op);
    }

    pub fn additionOp(self: *Vm, op: AdditionOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const rhs = self.stack.pop();
        defer rhs.deinit(self.allocator);
        const lhs = self.stack.pop();
        defer lhs.deinit(self.allocator);

        if (op == .concat) {
            if (lhs != .string)
                return self.generalError(op_loc, "expected indexable; found '{s}'", .{lhs.error_desc()});
            if (rhs != .string)
                return self.generalError(op_loc, "expected indexable; found '{s}'", .{rhs.error_desc()});
            self.stack.appendAssumeCapacity(.{
                .string = std.mem.concat(self.allocator, u8, &.{ lhs.string, rhs.string }) catch |e| oom(e),
            });
            return;
        }

        switch (lhs) {
            .bool => {},
            .number => |lhs_num| switch (rhs) {
                .bool => {},
                .number => |rhs_num| {
                    var result = std.math.big.int.Managed.init(self.allocator) catch |e| oom(e);
                    errdefer result.deinit();
                    switch (op) {
                        .concat => unreachable,
                        .add => {
                            result.ensureAddCapacity(lhs_num.toConst(), rhs_num.toConst()) catch |e| oom(e);
                            var m = result.toMutable();
                            m.add(lhs_num.toConst(), rhs_num.toConst());
                            result.setMetadata(m.positive, m.len);
                        },
                        .sub => {
                            result.ensureCapacity(@max(lhs_num.len, rhs_num.len) + 1) catch |e| oom(e);
                            var m = result.toMutable();
                            m.sub(lhs_num.toConst(), rhs_num.toConst());
                            result.setMetadata(m.positive, m.len);
                        },
                        .add_wrap, .sub_wrap,
                        .add_sat, .sub_sat,
                        => return self.generalError(
                            op_loc,
                            "zigscript doesn't support the {s} operator",
                            .{ op.str() },
                        ),
                    }
                    self.stack.appendAssumeCapacity(.{ .number = result.toMutable() });
                    return;
                },
                .string => {},
                .function => {},
            },
            .string => {},
            .function => {},
        }
        return self.generalError(
            op_loc,
            "invalid operands to binary expression '{s}' and '{s}'",
            .{ lhs.error_desc(), rhs.error_desc() }
        );
    }

    pub fn multiplyOp(self: *Vm, op: MultiplyOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const rhs = self.stack.pop();
        defer rhs.deinit(self.allocator);
        const lhs = self.stack.pop();
        defer lhs.deinit(self.allocator);

        switch (op) {
            .double_pipe => return self.tokenError(op_loc, .not_implemented),
            .mul => {
                if (lhs != .number or rhs != .number) return self.generalError(
                    op_loc,
                    "incompatible types: '{s}' and '{s}'",
                    .{lhs.error_desc(), rhs.error_desc()},
                );
                var result = std.math.big.int.Managed.init(self.allocator) catch |e| oom(e);
                errdefer result.deinit();
                result.ensureMulCapacity(lhs.number.toConst(), rhs.number.toConst()) catch |e| oom(e);
                var m = result.toMutable();
                m.mulNoAlias(lhs.number.toConst(), rhs.number.toConst(), self.allocator);
                result.setMetadata(m.positive, m.len);
                self.stack.appendAssumeCapacity(.{ .number = result.toMutable() });
            },
        }
    }

    pub fn applyPrefixOp(self: *Vm, op: PrefixOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 1); // should be guaranteed
        var value = self.stack.pop();
        var deinit_value = true;
        defer if (deinit_value) value.deinit(self.allocator);

        switch (op) {
            .not => {
                if (value != .bool) return self.generalError(
                    op_loc, "expected type 'bool' found '{s}'", .{@tagName(value)}
                );
                self.stack.appendAssumeCapacity(.{ .bool = !value.bool });
            },
            .negate => {
                if (value != .number) return self.generalError(
                    op_loc, "negation of type '{s}'", .{@tagName(value)}
                );
                deinit_value = false;
                value.number.negate();
                self.stack.appendAssumeCapacity(.{ .number = value.number });
            },
        }
    }

    pub fn applyOptional(self: *Vm, op_loc: usize) error{Vm}!void {
        return self.tokenError(op_loc, .not_implemented);
    }

    pub fn applyPtrType(self: *Vm, op_loc: usize) error{Vm}!void {
        _ = self;
        _ = op_loc;
        @panic("todo");
    }

    pub fn applyErrorUnion(self: *Vm, op_loc: usize) error{Vm}!void {
        return self.tokenError(op_loc, .not_implemented);
    }
};

const ValueType = enum {
    bool,
    number,
    string,
    function,
    pub fn error_desc(self: ValueType) []const u8 {
        return switch (self) {
            .bool => "bool",
            .number => "number",
            .string => "string",
            .function => "function",
        };
    }
};

const Value = union(ValueType) {
    bool: bool,
    number: std.math.big.int.Mutable,
    string: []const u8,
    function: Function,

    pub fn deinitAndInvalidate(self: *Value) void {
        self.deinit();
        self.* = undefined;
    }
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .bool => {},
            .number => |n| allocator.free(n.limbs),
            .string => |s| allocator.free(s),
            .function => |f| f.deinit(allocator),
        }
    }
    pub fn error_desc(self: Value) []const u8 {
        return @as(ValueType, self).error_desc();
    }
    pub fn clone(self: Value, allocator: std.mem.Allocator) Value {
        switch (self) {
            .bool => return self,
            .number => |n| {
                const copy = n.toManaged(allocator).clone() catch |e| oom(e);
                return .{ .number = copy.toMutable() };
            },
            .string => |s| return .{ .string = allocator.dupe(u8, s) catch |e| oom(e) },
            else => @panic("todo"),
        }
    }

    const Function = struct {
        id: ?std.zig.Token.Loc,
        params: []std.zig.Token.Loc,
        body: bool,
        pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
            allocator.free(self.params);
        }
    };
};

fn ErrorOr(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: []u8,
    };
}

fn compareOpStr(op: std.math.CompareOperator) []const u8 {
    return switch (op) {
        .lt => "<",
        .lte => "<=",
        .eq => "==",
        .gte => ">=",
        .gt => ">",
        .neq => "!=",
    };
}

pub const AdditionOp = enum {
    concat,
    add,
    sub,
    // TODO: start out not supporting these operations in zigscript
    add_wrap,
    sub_wrap,
    add_sat,
    sub_sat,
    pub fn str(self: AdditionOp) []const u8 {
        return switch (self) {
            .concat => "++",
            .add => "+",
            .sub => "-",
            .add_wrap => "+%",
            .sub_wrap => "-%",
            .add_sat => "+|",
            .sub_sat => "-|",
        };
    }
};

pub const MultiplyOp = enum {
    /// merges error sets or performs boolean OR
    double_pipe,
    mul,
    pub fn str(self: MultiplyOp) []const u8 {
        return switch (self) {
        };
    }
};

pub const PrefixOp = enum {
    not,
    negate,
};

fn testExpr(src: [:0]const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (interp.Expr(src, 0, &vm)) |end| {
        if (end != src.len) {
            std.log.err("src '{s}' is not an Expr (end={?})", .{src, end});
        }
        std.debug.assert(vm.scope_stack.items.len == 0);
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
    if (interp.Expr(src, 0, &vm)) |_| {
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

fn testBlock(src: [:0]const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (interp.Block(src, 0, &vm)) |end| {
        if (end != src.len) {
            std.log.err("src '{s}' is not a Block (end={?})", .{src, end});
        }
        std.debug.assert(vm.scope_stack.items.len == 0);
    } else |vm_err| switch (vm_err) {
        error.Vm => {
            const err = vm.err orelse @panic("vm reported error but has none?");
            const error_msg = err.getTestMsg();
            std.log.err("src '{s}' had unexpected error: {s}", .{src, error_msg});
            return error.TestUnexpectedResult;
        },
    }
}
fn testBlockError(src: [:0]const u8, expected_error: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (interp.Block(src, 0, &vm)) |_| {
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

fn testSrc(src: [:0]const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    try interp.Root(src, 0, &vm);
    std.debug.assert(vm.scope_stack.items.len == 0);
}

fn testSrcError(src: [:0]const u8, expected_error: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (interp.Root(src, 0, &vm)) |_| {
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
