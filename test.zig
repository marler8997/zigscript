const std = @import("std");
const Vm = @import("vm.zig").Vm;
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

    try testRoot("//! a doc comment");
    try testExpr("fn foo()");
    try testExpr("fn foo(bar)");
    try testExpr("fn foo(bar,)");
    try testExpr("fn foo(bar,baz)");
    try testExpr("fn foo(bar,baz,)");

    try testRoot("fn foo();");
    try testRoot("fn foo(){ }");

    try testBlock("{}");
    try testBlock("{@assert(true);}");
    try testBlockError("{@assert(true);@assert(false);}", "assert failed");
    try testBlock("{const a = 0;}");
    try testBlock("{const a = \"hello\";}");
    try testError("@assert(a == 0)", "undeclared identifier");
    try testBlockError("{const a = 0;const a = 0;}", "redeclaration");
    try testBlock("{const a = 0;@assert(a == 0);}");
    try testBlock("{var a = 0;@assert(a == 0);}");
    try testBlock("{var a = 1;@assert(a + 2 == 3);}");
    try testBlock("{var a = 3;@assert(12 == a * 4);}");
}

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

fn testRoot(src: [:0]const u8) !void {
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

fn testRootError(src: [:0]const u8, expected_error: []const u8) !void {
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
