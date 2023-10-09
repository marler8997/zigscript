const builtin = @import("builtin");
const std = @import("std");
const interp = @import("interp.zig");

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const TokenError = enum {
    undeclared_identifier,
    redeclaration,
    not_lvalue,
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
                .not_lvalue => return "invalid left-hand side to assignment",
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

pub const Mutability = enum { mutable, @"const" };

const ScopeEntry = struct {
    mutability: Mutability,
    value: Value,
};

const Scope = struct {
    map: std.StringHashMapUnmanaged(ScopeEntry) = .{},
    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        var it = self.map.iterator();
        while (it.next()) |pair| {
            pair.value_ptr.value.deinit(allocator);
        }
        self.map.deinit(allocator);
    }
};

pub const Container = struct {
    src: [:0]const u8,
    allocator: std.mem.Allocator,
    err: ?VmError = null,
    map: std.StringHashMapUnmanaged(ScopeEntry) = .{},
    current_function: ?Function = null,

    const Function = struct {
        id: ?std.zig.Token.Loc,
        params: std.ArrayListUnmanaged(std.zig.Token.Loc) = .{},
        params_done: bool = false,
    };

    pub fn deinit(self: *Container) void {
        if (self.err) |err| {
            err.deinit(self.allocator);
        }
        if (self.current_function) |*f| {
            f.params.deinit(self.allocator);
        }
        {
            var it = self.map.iterator();
            while (it.next()) |pair| {
                pair.value_ptr.value.deinit(self.allocator);
            }
        }
        self.map.deinit(self.allocator);
    }
    pub fn functionProtoStart(self: *Container, id: ?std.zig.Token.Loc) void {
        if (self.current_function) |_|
            @panic("function proto inside function proto not implemented");
        self.current_function = .{ .id = id };
    }
    pub fn functionProtoEndParams(self: *Container) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (f.params_done) @panic("codebug");
        f.params_done = true;
    }
    pub fn functionProtoParam(self: *Container, id: std.zig.Token.Loc) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (f.params_done) @panic("codebug");
        f.params.append(self.allocator, id) catch |e| oom(e);
    }

    pub fn functionProtoNoBody(self: *Container) error{Vm}!void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (!f.params_done) @panic("codebug");

        const id = f.id orelse return "missing function name";

        const slice = self.src[id.start .. id.end];
        if (self.scopeLookup(slice)) |_| return self.tokenError(id_loc.start, .redeclaration);
        if (self.scope_stack.items.len == 0) @panic("todo: assign var in global scope");
        const scope = &self.scope_stack.items[self.scope_stack.items.len-1];
        const value = (self.stack.popOrNull() orelse @panic("codebug")).resolve(self.*);
        scope.map.putNoClobber(self.allocator, slice, .{
            .mutability = mutability,
            .value = value,
        }) catch |e| oom(e);


        !!!

        self.stack.append(self.allocator, .{ .direct = .{ .function = .{
            .id = f.id,
            .params = f.params.toOwnedSlice(self.allocator) catch |e| oom(e),
            .body = false,
        }}}) catch |e| oom(e);
        self.current_function = null;
    }
    pub fn functionProtoBodyStart(self: *Container) void {
        const f = &(self.current_function orelse @panic("codebug"));
        if (!f.params_done) @panic("codebug");
        self.stack.append(self.allocator, .{ .direct = .{ .function = .{
            .id = f.id,
            .params = f.params.toOwnedSlice(self.allocator) catch |e| oom(e),
            .body = true,
        }}}) catch |e| oom(e);
        self.current_function = null;
    }
    pub fn functionProtoBodyFailedParse(self: *Container) void {
        if (self.stack.items.len == 0) @panic("codebug");
        const f = self.stack.pop().resolve(self.*);
        std.debug.assert(f == .function);
    }
    pub fn functionProtoBodyEnd(self: *Container) void {
        // TODO: set the last block parsed to the function
        _ = self;
    }

};

pub const Vm = struct {
    src: [:0]const u8,
    allocator: std.mem.Allocator,
    stack: std.ArrayListUnmanaged(IndirectValue) = .{},
    err: ?VmError = null,
    scope_stack: std.ArrayListUnmanaged(Scope) = .{},

    pub fn deinit(self: *Vm) void {
        if (self.err) |err| {
            err.deinit(self.allocator);
        }
        for (self.stack.items) |item| {
            item.deinit(self.allocator);
        }
        self.stack.deinit(self.allocator);
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
        self.stack.append(self.allocator, .{ .direct = .{ .bool = val } }) catch |e| oom(e);
    }

    pub fn push_string_literal(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const token = self.src[loc.start..loc.end];
        const string = std.zig.string_literal.parseAlloc(self.allocator, token) catch |err| switch (err) {
            error.OutOfMemory => |e| oom(e),
            error.InvalidLiteral => return self.tokenError(loc.start, .invalid_string_literal),
        };
        self.stack.append(self.allocator, .{ .direct = .{ .string = string } }) catch |e| oom(e);
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

        self.stack.append(self.allocator, .{ .direct = .{
            .number = num.toMutable(),
        }}) catch |e| oom(e);
    }

    pub fn pushCharLiteral(self: *Vm, token: std.zig.Token) error{Vm}!void {
        return self.tokenError(token.loc.start, .not_implemented);
    }

    pub fn pushDotIdentifier(self: *Vm, token: std.zig.Token) error{Vm}!void {
        return self.tokenError(token.loc.start, .not_implemented);
    }

    const ScopeLookup = struct {
        scope_index: usize,
        entry: *const ScopeEntry,
    };
    fn scopeLookup(self: *Vm, slice: []const u8) ?ScopeLookup {
        var i: usize = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            const scope = &self.scope_stack.items[i];
            if (scope.map.get(slice)) |*value_ptr|
                return .{ .scope_index = i, .entry = value_ptr };
        }
        return null;
    }

    pub fn getStackTopLValue(self: *Vm, token_pos: usize) error{Vm}!ScopeRef {
        if (self.stack.items.len == 0) @panic("codebug");
        const top = self.stack.pop();
        return switch (top) {
            .direct => self.tokenError(token_pos, .not_lvalue),
            .scope_ref => |s| s,
        };
    }

    pub fn assignStackTop(self: *Vm, op_pos: usize, op: AssignOp, l_value: ScopeRef) error{Vm}!void {
        const entry = l_value.getEntry(self.*);
        var rhs_indirect = self.stack.popOrNull() orelse @panic("codebug");
        errdefer rhs_indirect.deinit(self.allocator);
        switch (op) {
            .normal => rhs_indirect.moveInto(self.allocator, &entry.value) catch return self.generalError(
                op_pos,
                "expected type '{s}', found '{s}'",
                .{ entry.value.error_desc(), rhs_indirect.resolve(self.*).error_desc() },
            ),
        }
    }

    pub fn declareAndAssignStackTop(self: *Vm, mutability: Mutability, id_loc: std.zig.Token.Loc) error{Vm}!void {
        const slice = self.src[id_loc.start .. id_loc.end];
        if (self.scopeLookup(slice)) |_| return self.tokenError(id_loc.start, .redeclaration);
        if (self.scope_stack.items.len == 0) @panic("todo: assign var in global scope");
        const scope = &self.scope_stack.items[self.scope_stack.items.len-1];
        const value = (self.stack.popOrNull() orelse @panic("codebug")).resolve(self.*);
        scope.map.putNoClobber(self.allocator, slice, .{
            .mutability = mutability,
            .value = value,
        }) catch |e| oom(e);
    }

    pub fn pushID(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const slice = self.src[loc.start..loc.end];
        const lookup = self.scopeLookup(slice) orelse return self.tokenError(loc.start, .undeclared_identifier);
        self.stack.append(self.allocator, .{ .scope_ref = .{
            .scope_index = lookup.scope_index,
            .id = loc,
        }}) catch |e| oom(e);
    }

    fn enforceArgCount(self: *Vm, loc: usize, count: usize) error{Vm}!void {
        if (self.stack.items.len != count) return self.generalError(
            loc,
            "expected {} argument(s), found {}",
            .{count, self.stack.items.len},
        );
    }

    fn enforceType(self: *Vm, comptime expected_type: ValueType, loc: usize, value: Value) error{Vm}!expected_type.T() {
        if (value != expected_type) return self.generalError(
            loc,
            "expected type '{s}', found '{s}'",
            .{ expected_type.error_desc(), value.error_desc() },
        );
        return switch (expected_type) {
            .bool => value.bool,
            .number => value.number,
            .string => value.string,
            .function => value.function,
        };
    }

    pub fn callBuiltin(self: *Vm, loc: std.zig.Token.Loc) error{Vm}!void {
        const name = self.src[loc.start..loc.end];
        if (std.mem.eql(u8, name, "@out")) {
            try self.enforceArgCount(loc.start, 1);
            const msg_indirect = self.stack.pop();
            defer msg_indirect.deinit(self.allocator);
            const msg = try self.enforceType(.string, loc.start, msg_indirect.resolve(self.*));
            std.io.getStdOut().writer().writeAll(msg)
                catch |err| std.debug.panic("@out failed with {s}", .{@errorName(err)});
        } else if (std.mem.eql(u8, name, "@assert")) {
            try self.enforceArgCount(loc.start, 1);
            const val_indirect = self.stack.pop();
            defer val_indirect.deinit(self.allocator);
            const val = try self.enforceType(.bool, loc.start, val_indirect.resolve(self.*));
            if (!val)
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
        const first_indirect = self.stack.pop();
        defer first_indirect.deinit(self.allocator);
        const second_indirect = self.stack.pop();
        defer second_indirect.deinit(self.allocator);
        const first = try self.enforceType(.bool, op_loc, first_indirect.resolve(self.*));
        const second = try self.enforceType(.bool, op_loc, second_indirect.resolve(self.*));
        // we know we have capacity because we just popped off the old values
        self.stack.appendAssumeCapacity(.{ .direct = .{
            .bool = switch (op) {
                .@"or" => first or second,
                .@"and" => first and second,
            },
        }});
    }
    pub fn binaryCompareOp(self: *Vm, op: std.math.CompareOperator, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 2); // should be guaranteed
        const rhs_indirect = self.stack.pop();
        defer rhs_indirect.deinit(self.allocator);
        const lhs_indirect = self.stack.pop();
        defer lhs_indirect.deinit(self.allocator);
        const rhs = rhs_indirect.resolve(self.*);
        const lhs = lhs_indirect.resolve(self.*);

        if (lhs == .string or rhs == .string)
            return self.generalError(op_loc, "cannot compare strings with {s}", .{compareOpStr(op)});

        // we know we have capacity because we just popped off the old values
        self.stack.appendAssumeCapacity(.{ .direct = .{
            .bool = switch (lhs) {
                .bool => |v| try self.compareBool(op, op_loc, v, rhs),
                .number => |v| try self.compareNumber(op, op_loc, v, rhs),
                .string => unreachable,
                .function => @panic("todo"),
            }
        }});
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
        const rhs_indirect = self.stack.pop();
        defer rhs_indirect.deinit(self.allocator);
        const lhs_indirect = self.stack.pop();
        defer lhs_indirect.deinit(self.allocator);
        const rhs = rhs_indirect.resolve(self.*);
        const lhs = lhs_indirect.resolve(self.*);

        if (op == .concat) {
            if (lhs != .string)
                return self.generalError(op_loc, "expected indexable; found '{s}'", .{lhs.error_desc()});
            if (rhs != .string)
                return self.generalError(op_loc, "expected indexable; found '{s}'", .{rhs.error_desc()});
            self.stack.appendAssumeCapacity(.{ .direct = .{
                .string = std.mem.concat(self.allocator, u8, &.{ lhs.string, rhs.string }) catch |e| oom(e),
            }});
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
                    self.stack.appendAssumeCapacity(.{ .direct = .{ .number = result.toMutable() } });
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
        const rhs_indirect = self.stack.pop();
        defer rhs_indirect.deinit(self.allocator);
        const lhs_indirect = self.stack.pop();
        defer lhs_indirect.deinit(self.allocator);
        const rhs = rhs_indirect.resolve(self.*);
        const lhs = lhs_indirect.resolve(self.*);

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
                self.stack.appendAssumeCapacity(.{ .direct = .{ .number = result.toMutable() } });
            },
        }
    }

    pub fn applyPrefixOp(self: *Vm, op: PrefixOp, op_loc: usize) error{Vm}!void {
        std.debug.assert(self.stack.items.len >= 1); // should be guaranteed
        var value_indirect = self.stack.pop();
        var deinit_value = true;
        defer if (deinit_value) value_indirect.deinit(self.allocator);
        var value = value_indirect.resolve(self.*);

        switch (op) {
            .not => {
                if (value != .bool) return self.generalError(
                    op_loc, "expected type 'bool' found '{s}'", .{@tagName(value)}
                );
                self.stack.appendAssumeCapacity(.{ .direct = .{ .bool = !value.bool } });
            },
            .negate => {
                if (value != .number) return self.generalError(
                    op_loc, "negation of type '{s}'", .{@tagName(value)}
                );
                deinit_value = false;
                value.number.negate();
                self.stack.appendAssumeCapacity(.{ .direct = .{ .number = value.number } });
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

pub const ScopeRef = struct {
    scope_index: usize,
    id: std.zig.Token.Loc,
    pub fn deinit(self: ScopeRef, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
    pub fn getEntry(self: ScopeRef, vm: Vm) *ScopeEntry {
        std.debug.assert(self.scope_index < vm.scope_stack.items.len);
        const slice = vm.src[self.id.start .. self.id.end];
        const scope = &vm.scope_stack.items[self.scope_index];
        return scope.map.getPtr(slice) orelse @panic("codebug");
    }
};

const IndirectValue = union(enum) {
    direct: Value,
    scope_ref: ScopeRef,

    pub fn deinit(self: IndirectValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .direct => |d| d.deinit(allocator),
            .scope_ref => |s| s.deinit(allocator),
        }
    }

    pub fn resolve(self: IndirectValue, vm: Vm) Value {
        return switch (self) {
            .direct => |d| d,
            .scope_ref => |s| s.getEntry(vm).value,
        };
    }

    pub fn moveInto(self: *IndirectValue, allocator: std.mem.Allocator, dst: *Value) error{TypeMismatch}!void {
        switch (self.*) {
            .direct => |*d| try dst.move(allocator, d),
            .scope_ref => {
                @panic("todo");
            },
        }
    }
};

const ValueType = enum {
    bool,
    number,
    string,
    function,
    pub fn T(comptime self: ValueType) type {
        return switch (self) {
            .bool => bool,
            .number => std.math.big.int.Mutable,
            .string => []const u8,
            .function => Value.Function,
        };
    }
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
    pub fn move(self: *Value, allocator: std.mem.Allocator, from: *Value) error{TypeMismatch}!void {
        switch (self.*) {
            .bool => {
                const from_bool = switch (from.*) {
                    .bool => |b2| b2,
                    else => return error.TypeMismatch,
                };
                self.* = .{ .bool = from_bool };
            },
            .number => |n| {
                const from_num = switch (from.*) {
                    .number => |n2| n2,
                    else => return error.TypeMismatch,
                };
                allocator.free(n.limbs);
                self.* = .{ .number = from_num };
            },
            .string => |s| {
                const from_str = switch (from.*) {
                    .string => |s2| s2,
                    else => return error.TypeMismatch,
                };
                allocator.free(s);
                self.* = .{ .string = from_str };
            },
            .function => @panic("todo"),
        }
        from.* = undefined;
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

pub const AssignOp = enum {
    normal,
};

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
