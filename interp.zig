const std = @import("std");
const zigscript = @import("zigscript.zig");
const Vm = zigscript.Vm;
const AdditionOp = zigscript.AdditionOp;
const MultiplyOp = zigscript.MultiplyOp;
const PrefixOp = zigscript.PrefixOp;

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

// --------------------------------------------------------------------------------
// GRAMMAR FUNCTIONS
// --------------------------------------------------------------------------------

// Expr <- BoolOrExpr
const Expr = BoolOrExpr;

// BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*
fn BoolOrExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const first_expr_end = try BoolAndExpr(src, start, vm_opt) orelse return null;

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
    const first_expr_end = try CompareExpr(src, start, vm_opt) orelse return null;

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
    const first_expr_end = try BitwiseExpr(src, start, vm_opt) orelse return null;

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
    const first_expr_end = try MultiplyExpr(src, start, vm_opt) orelse return null;

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
    const first_expr_end = try PrefixExpr(src, start, vm_opt) orelse return null;

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

    const expr_end = try PrimaryExpr(src, prefix_end, vm_opt) orelse return null;
    if (vm_opt) |vm| {
        try applyPrefixOps(src, start, op_count, vm);
    }
    return expr_end;
}

// PrimaryExpr
//     <- AsmExpr
//      / IfExpr
//      / KEYWORD_break BreakLabel? Expr?
//      / KEYWORD_comptime Expr
//      / KEYWORD_nosuspend Expr
//      / KEYWORD_continue BreakLabel?
//      / KEYWORD_resume Expr
//      / KEYWORD_return Expr?
//      / BlockLabel? LoopExpr
//      / Block
//      / CurlySuffixExpr
fn PrimaryExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);

    if (token.tag == .keyword_asm) {
        @panic("AsmExpr not implemented");
    } else if (token.tag == .keyword_if) {
        @panic("IfExpr not implemented");
    } else if (token.tag == .keyword_break) {
        @panic("break PrimaryExpr not implemented");
    } else if (token.tag == .keyword_nosuspend) {
        @panic("nosuspend not implemented");
    } else if (token.tag == .keyword_continue) {
        @panic("continue not implemented");
    } else if (token.tag == .keyword_resume) {
        @panic("resume not implemented");
    } else if (token.tag == .keyword_return) {
        @panic("return not implemented");
    } else {
        {
            const loop_expr_start = blk: {
                if (BlockLabel(src, start, null)) |end| break :blk end;
                break :blk start;
            };
            if (try LoopExpr(src, loop_expr_start, vm_opt)) |loop_end| {
                _ = loop_end;
                @panic("TODO: loop expr");
            }
        }

        if (try Block(src, start, vm_opt)) |block_end| {
            _ = block_end;
            @panic("Block PrimaryExpr not implemented");
        }

        return CurlySuffixExpr(src, start, vm_opt);
    }
}

// IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?

// Block <- LBRACE Statement* RBRACE
fn Block(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const statement_start = blk: {
        const token = lex(src, start);
        if (token.tag != .l_brace) return null;
        break :blk token.loc.end;
    };
    _ = vm_opt;
    _ = statement_start;
    @panic("todo: Block");
}

// LoopExpr <- KEYWORD_inline? (ForExpr / WhileExpr)
fn LoopExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    var token = lex(src, start);
    const loop_start = blk: {
        if (token.tag == .keyword_inline) {
            token = lex(src, token.loc.end);
            break :blk token.loc.end;
        }
        break :blk start;
    };

    if (try ForExpr(src, loop_start, vm_opt)) |for_end| {
        _ = for_end;
        @panic("todo: ForExpr");
    } else if (try WhileExpr(src, loop_start, vm_opt)) |while_end| {
        _ = while_end;
        @panic("todo: WhileExpr");
    } else return null;
}

// ForPrefix <- KEYWORD_for LPAREN ForArgumentsList RPAREN PtrListPayload
fn ForExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    {
        var token = lex(src, start);
        if (token.tag != .keyword_for) return null;
    }
    _ = vm_opt;
    @panic("todo");
}

// WhileExpr <- WhilePrefix Expr (KEYWORD_else Payload? Expr)?
fn WhileExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    {
        var token = lex(src, start);
        if (token.tag != .keyword_while) return null;
    }
    _ = vm_opt;
    @panic("todo");
}

// CurlySuffixExpr <- TypeExpr InitList?
fn CurlySuffixExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
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
pub fn PrimaryTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) !usize {
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

// BlockLabel <- IDENTIFIER COLON
fn BlockLabel(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    const id_token = lex(src, start);
    if (id_token.tag != .identifier) return null;

    const colon_token = lex(src, id_token.loc.end);
    if (colon_token.tag != .colon) return null;

    if (vm_opt) |vm| {
        _ = vm;
        @panic("TODO: register block label with vm");
    }
    return colon_token.loc.end;
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
