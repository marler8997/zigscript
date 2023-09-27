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
}

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const TokenError = enum {
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
                .not_implemented => return "not implemented",
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
            },
            .string => {},
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
    number: std.math.big.int.Mutable,
    string: []const u8,

    pub fn deinitAndInvalidate(self: *Value) void {
        self.deinit();
        self.* = undefined;
    }
    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .bool => {},
            .number => |n| allocator.free(n.limbs),
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
fn asCompareOp(token: std.zig.Token) ?std.math.CompareOperator {
    return switch (token.tag) {
        .equal_equal => .eq,
        .bang_equal => .neq,
        .angle_bracket_left => .lt,
        .angle_bracket_right => .gt,
        .angle_bracket_left_equal => .lte,
        .angle_bracket_right_equal => .gte,
        else => return null,
    };
}

const AdditionOp = enum {
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
fn asAdditionOp(token: std.zig.Token) ?AdditionOp {
    return switch (token.tag) {
        .plus_plus => return .concat,
        .plus => return .add,
        .minus => return .sub,
        .plus_percent => return .add_wrap,
        .minus_percent => return .sub_wrap,
        .plus_pipe => return .add_sat,
        .minus_pipe => return .sub_sat,
        else => return null,
    };
}

const MultiplyOp = enum {
    /// merges error sets or performs boolean OR
    double_pipe,
    mul,
    pub fn str(self: MultiplyOp) []const u8 {
        return switch (self) {
        };
    }
};
fn asMultiplyOp(token: std.zig.Token) ?MultiplyOp {
    return switch (token.tag) {
        .pipe_pipe => .double_pipe,
        .asterisk => .mul,
        .slash => @panic("todo"),
        .percent => @panic("todo"),
        .asterisk_asterisk => @panic("todo"),
        .asterisk_percent => @panic("todo"),
        .asterisk_pipe => @panic("todo"),
        else => return null,
    };
}

const PrefixOp = enum {
    not,
    negate,
};
fn asPrefixOp(token: std.zig.Token) ?PrefixOp {
    return switch (token.tag) {
        .bang => .not,
        .minus => .negate,
        .tilde => @panic("todo"),
        .minus_percent => @panic("todo"),
        .ampersand => @panic("todo"),
        .keyword_try => @panic("todo"),
        .keyword_await => @panic("todo"),
        else => return null,
    };
}
fn applyPrefixOps(src: [:0]const u8, start: usize, op_count: u16, vm: *Vm) !void {
    var off = start;
    for (0 .. op_count) |_| {
        const token = lex(src, off);
        const op = asPrefixOp(token) orelse unreachable;
        try vm.applyPrefixOp(op, token.loc.start);
        off = token.loc.end;
    }
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
const BitwiseExpr = BitShiftExpr; // TODO
// BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*
const BitShiftExpr = AdditionExpr; // TODO

// AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*
fn AdditionExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const first_expr_end = try MultiplyExpr(src, start, null) orelse return null;
    if (vm_opt) |vm|
        std.debug.assert(first_expr_end == try MultiplyExpr(src, start, vm));

    var off = first_expr_end;
    while (true) {
        const token = lex(src, off);
        const op = asAdditionOp(token) orelse break;

        const expr_end = try MultiplyExpr(src, token.loc.end, null) orelse break;
        if (vm_opt) |vm| {
            std.debug.assert(expr_end == try MultiplyExpr(src, token.loc.end, vm));
            try vm.additionOp(op, token.loc.start);
        }

        off = expr_end;
    }
    return off;
}

// MultiplyExpr <- PrefixExpr (MultiplyOp PrefixExpr)*
fn MultiplyExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const first_expr_end = try PrefixExpr(src, start, null) orelse return null;
    if (vm_opt) |vm|
        std.debug.assert(first_expr_end == try PrefixExpr(src, start, vm));

    var off = first_expr_end;
    while (true) {
        const token = lex(src, off);
        const op = asMultiplyOp(token) orelse break;

        const expr_end = try PrefixExpr(src, token.loc.end, null) orelse break;
        if (vm_opt) |vm| {
            std.debug.assert(expr_end == try PrefixExpr(src, token.loc.end, vm));
            try vm.multiplyOp(op, token.loc.start);
        }

        off = expr_end;
    }
    return off;
}

// PrefixExpr <- PrefixOp* PrimaryExpr
fn PrefixExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {

    var op_count: u16 = 0;
    const prefix_end = blk: {
        var off = start;
        while (true) {
            const token = lex(src, off);
            _ = asPrefixOp(token) orelse break :blk off;
            off = token.loc.end;
            op_count += 1;
        }
    };

    const expr_end = try PrimaryExpr(src, prefix_end, null) orelse return null;
    if (vm_opt) |vm| {
        std.debug.assert(expr_end == try PrimaryExpr(src, prefix_end, vm));
        try applyPrefixOps(src, start, op_count, vm);
    }
    return expr_end;
}

fn PrimaryExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
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
