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

fn applyPrefixOps(src: [:0]const u8, start: usize, op_count: u16, vm: *Vm) error{Vm}!void {
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

// FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? AddrSpace? LinkSection? CallConv? EXCLAMATIONMARK? TypeExpr
fn FnProto(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const after_fn = blk: {
        const token = lex(src, start);
        if (token.tag != .keyword_fn) return null;
        break :blk token.loc.end;
    };
    _ = after_fn;
    _ = vm_opt;
    @panic("todo");
}

// Expr <- BoolOrExpr
pub const Expr = BoolOrExpr;

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
    const expr_end = try TypeExpr(src, start, vm_opt) orelse return null;
    if (try InitList(src, expr_end, vm_opt)) |init_list_end|
        return init_list_end;
    return expr_end;
}

// InitList
//     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
//      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
//      / LBRACE RBRACE
fn InitList(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag != .l_brace)
        return null;
    _ = vm_opt;
    @panic("todo");
}

// TypeExpr <- PrefixTypeOp* ErrorUnionExpr
fn TypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    var off = start;
    while (true) {
        off = try PrefixTypeOp(src, off, vm_opt) orelse break;
    }
    return ErrorUnionExpr(src, off, vm_opt);
}

// ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
fn ErrorUnionExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const expr_end = try SuffixExpr(src, start, vm_opt) orelse return null;

    const token = lex(src, expr_end);
    if (token.tag == .bang) {
        if (try TypeExpr(src, token.loc.end, vm_opt)) |end| {
            if (vm_opt) |vm| try vm.applyErrorUnion(token.loc.start);
            return end;
        }
    }
    return expr_end;
}

// SuffixExpr
//     <- KEYWORD_async PrimaryTypeExpr SuffixOp* FnCallArguments
//      / PrimaryTypeExpr (SuffixOp / FnCallArguments)*
fn SuffixExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag == .keyword_async)
        @panic("async not implemented");

    var off = try PrimaryTypeExpr(src, start, vm_opt) orelse return null;
    while (true) {
        if (try SuffixOp(src, off, vm_opt)) |end| {
            off = end;
            @panic("TODO: apply SuffixOp");
        } else if (try FnCallArguments(src, off, vm_opt)) |end| {
            off = end;
            @panic("TODO: apply FnCallArguments");
        } else break;
    }
    return off;
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
fn PrimaryTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
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
    } else if (first_token.tag == .char_literal) {
        if (vm_opt) |vm| try vm.pushCharLiteral(first_token);
        return first_token.loc.end;
    } else if (try ContainerDecl(src, start, vm_opt)) |decl_end| {
        return decl_end;
    }

    if (first_token.tag == .period) {
        const second_token = lex(src, first_token.loc.end);
        if (second_token.tag == .identifier) {
            if (vm_opt) |vm| try vm.pushDotIdentifier(second_token);
            return second_token.loc.end;
        } else if (try InitList(src, first_token.loc.end, vm_opt)) |end| {
            _ = end;
            @panic("b");
        }
    }

    if (try ErrorSetDecl(src, start, vm_opt)) |end|
        return end;

    if (first_token.tag == .number_literal) {
        if (vm_opt) |vm| try vm.push_number_literal(first_token.loc);
        return first_token.loc.end;
    }

    if (try FnProto(src, start, vm_opt)) |end|
        return end;

    if (try GroupedExpr(src, start, vm_opt)) |end|
        return end;

    if (first_token.tag == .identifier) {
        const slice = src[first_token.loc.start..first_token.loc.end];
        if (vm_opt) |vm| {
            if (std.mem.eql(u8, slice, "false")) {
                vm.push_bool(false);
            } else if (std.mem.eql(u8, slice, "true")) {
                vm.push_bool(true);
            } else {
                return vm.tokenError(first_token.loc.start, .not_implemented);
            }
        }
        return first_token.loc.end;
    }

    if (try IfTypeExpr(src, start, vm_opt)) |end|
        return end;

    if (first_token.tag == .keyword_comptime)
        return TypeExpr(src, first_token.loc.end, vm_opt);

    // TODO: KEYWORD_error DOT IDENTIFIER
    if (first_token.tag == .keyword_error)
        @panic("todo");

    if (first_token.tag == .keyword_anyframe)
        @panic("todo: anyframe");

    if (first_token.tag == .keyword_unreachable)
        @panic("todo: unreachable");

    if (first_token.tag == .string_literal) {
        if (vm_opt) |vm| try vm.push_string_literal(first_token.loc);
        return first_token.loc.end;
    }

    return SwitchExpr(src, start, vm_opt);
}

// ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto
fn ContainerDecl(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    var modifier: ?enum { @"extern", @"packed" } = null;

    const decl_start = blk: {
        const token = lex(src, start);
        if (token.tag == .keyword_extern) {
            modifier = .@"extern";
            break :blk token.loc.end;
        }
        if (token.tag == .keyword_packed) {
            modifier = .@"packed";
            break :blk token.loc.end;
        }
        break :blk start;
    };

    const end = try ContainerDeclAuto(src, decl_start, vm_opt) orelse return null;
    if (vm_opt) |vm| {
        _ = vm;
        if (modifier) |_| @panic("todo: apply container modifier to vm type");
    }
    return end;
}

// ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE
fn ErrorSetDecl(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag != .keyword_error)
        return null;
    _ = vm_opt;
    @panic("todo");
}

// GroupedExpr <- LPAREN Expr RPAREN
fn GroupedExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const expr_start = blk: {
        const token = lex(src, start);
        if (token.tag != .l_paren) return null;
        break :blk token.loc.end;
    };

    const expr_end = try Expr(src, expr_start, null) orelse return null;

    const token = lex(src, expr_end);
    if (token.tag != .r_paren) return null;

    if (vm_opt) |vm| {
        const new_end = try Expr(src, expr_start, vm) orelse unreachable;
        std.debug.assert(expr_end == new_end);
    }
    return token.loc.end;
}

// IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
fn IfTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const prefix_end = IfPrefix(src, start, null) orelse return null;
    _ = prefix_end;
    _ = vm_opt;
    @panic("todo");
}

// LabeledTypeExpr
//     <- BlockLabel Block
//      / BlockLabel? LoopTypeExpr
fn LabeledTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    const label_end = blk: {
        if (BlockLabel(src, start, vm_opt)) |end| {
            std.debug.assert(end != start);
            break :blk end;
        }
        break :blk start;
    };
    defer {
        if (label_end != start) {
            if (vm_opt) |vm| vm.popBlockLabel(lex(src, start));
        }
    }
    if (label_end != start) {
        if (Block(src, label_end, vm_opt)) |end|
            return end;
    }
    return LoopTypeExpr(src, label_end, vm_opt);
}

// LoopTypeExpr <- KEYWORD_inline? (ForTypeExpr / WhileTypeExpr)
fn LoopTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    var token = lex(src, start);
    const loop_start = blk: {
        if (token.tag == .keyword_inline) {
            token = lex(src, token.loc.end);
            break :blk token.loc.end;
        }
        break :blk start;
    };

    if (try ForTypeExpr(src, loop_start, vm_opt)) |for_end| {
        _ = for_end;
        @panic("todo: ForTypeExpr");
    } else if (try WhileTypeExpr(src, loop_start, vm_opt)) |while_end| {
        _ = while_end;
        @panic("todo: WhileTypeExpr");
    } else return null;
}

// ForTypeExpr <- ForPrefix TypeExpr (KEYWORD_else TypeExpr)?
fn ForTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    if (try ForPrefix(src, start, null))
        @panic("todo");
    _ = vm_opt;
    return null;
}

// WhileTypeExpr <- WhilePrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
fn WhileTypeExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    if (try WhilePrefix(src, start, null))
        @panic("todo");
    _ = vm_opt;
    return null;
}

// SwitchExpr <- KEYWORD_switch LPAREN Expr RPAREN LBRACE SwitchProngList RBRACE
fn SwitchExpr(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const expr_start = blk: {
        const first_token = lex(src, start);
        if (first_token.tag != .keyword_switch)
            return null;
        const second_token = lex(src, first_token.loc.end);
        if (second_token.tag != .l_paren)
            return null;
        break :blk second_token.loc.end;
    };
    _ = expr_start;
    _ = vm_opt;
    @panic("todo");
}

// BlockLabel <- IDENTIFIER COLON
fn BlockLabel(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    const id_token = lex(src, start);
    if (id_token.tag != .identifier) return null;

    const colon_token = lex(src, id_token.loc.end);
    if (colon_token.tag != .colon) return null;

    if (vm_opt) |vm| {
        vm.pushBlockLabel(id_token.loc);
    }
    return colon_token.loc.end;
}

// IfPrefix <- KEYWORD_if LPAREN Expr RPAREN PtrPayload?
fn IfPrefix(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    {
        var token = lex(src, start);
        if (token.tag != .keyword_if) return null;
    }
    _ = vm_opt;
    @panic("todo");
}

// WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?
fn WhilePrefix(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    {
        var token = lex(src, start);
        if (token.tag != .keyword_while) return null;
    }
    _ = vm_opt;
    @panic("todo");
}

// ForPrefix <- KEYWORD_for LPAREN ForArgumentsList RPAREN PtrListPayload
fn ForPrefix(src: [:0]const u8, start: usize, vm_opt: ?*Vm) ?usize {
    {
        var token = lex(src, start);
        if (token.tag != .keyword_for) return null;
    }
    _ = vm_opt;
    @panic("todo");
}

// PrefixTypeOp
//     <- QUESTIONMARK
//      / KEYWORD_anyframe MINUSRARROW
//      / SliceTypeStart (ByteAlign / AddrSpace / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
//      / PtrTypeStart (AddrSpace / KEYWORD_align LPAREN Expr (COLON Expr COLON Expr)? RPAREN / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
//      / ArrayTypeStart
fn PrefixTypeOp(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const first_token = lex(src, start);
    if (first_token.tag == .question_mark) {
        if (vm_opt) |vm|
            try vm.applyOptional(first_token.loc.start);
        return first_token.loc.end;
    } else if (first_token.tag == .keyword_anyframe) {
        @panic("todo");
    } else if (try SliceTypeStart(src, start, vm_opt)) |slice_type_end| {
        _ = slice_type_end;
        @panic("todo");
    } else if (try PtrTypeStart(src, start, vm_opt)) |ptr_type_end| {
        _ = ptr_type_end;
        @panic("todo");
    } else if (try ArrayTypeStart(src, start, vm_opt)) |end| {
        return end;
    } else return null;
}

// SuffixOp
//     <- LBRACKET Expr (DOT2 (Expr? (COLON Expr)?)?)? RBRACKET
//      / DOT IDENTIFIER
//      / DOTASTERISK
//      / DOTQUESTIONMARK
fn SuffixOp(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    _ = vm_opt;
    const token = lex(src, start);
    if (token.tag == .l_bracket) {
        @panic("todo");
    } else if (token.tag == .period) {
        @panic("todo");
    } else if (token.tag == .period_asterisk) {
        @panic("todo");
    // don't see this token in tokenizer.zig
    //} else if (token.tag == .period_question_mark) {
    //    @panic("todo");
    } else return null;
}

// FnCallArguments <- LPAREN ExprList RPAREN
fn FnCallArguments(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
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

// SliceTypeStart <- LBRACKET (COLON Expr)? RBRACKET
fn SliceTypeStart(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag != .l_bracket)
        return null;
    _ = vm_opt;
    @panic("todo");
}

// PtrTypeStart
//     <- ASTERISK
//      / ASTERISK2
//      / LBRACKET ASTERISK (LETTERC / COLON Expr)? RBRACKET
fn PtrTypeStart(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag == .asterisk) {
        if (vm_opt) |vm| try vm.applyPtrType(token.loc.start);
        return token.loc.end;
    } else if (token.tag == .asterisk_asterisk) {
        @panic("what is the '**' prefix type operator?");
    } else if (token.tag == .l_bracket) {
        @panic("todo");
    } else return null;
}

// ArrayTypeStart <- LBRACKET Expr (COLON Expr)? RBRACKET
fn ArrayTypeStart(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const token = lex(src, start);
    if (token.tag != .l_bracket)
        return null;
    _ = vm_opt;
    @panic("todo");
}

// ContainerDeclAuto <- ContainerDeclType LBRACE container_doc_comment? ContainerMembers RBRACE
fn ContainerDeclAuto(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    const decl_type_end = try ContainerDeclType(src, start, null) orelse return null;
    _ = decl_type_end;
    _ = vm_opt;
    @panic("todo");
}

// ContainerDeclType
//     <- KEYWORD_struct (LPAREN Expr RPAREN)?
//      / KEYWORD_opaque
//      / KEYWORD_enum (LPAREN Expr RPAREN)?
//      / KEYWORD_union (LPAREN (KEYWORD_enum (LPAREN Expr RPAREN)? / Expr) RPAREN)?
fn ContainerDeclType(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!?usize {
    _ = vm_opt;
    const first_token = lex(src, start);
    if (first_token.tag == .keyword_struct) {
        @panic("todo");
    } else if (first_token.tag == .keyword_opaque) {
        @panic("todo");
    } else if (first_token.tag == .keyword_enum) {
        @panic("todo");
    } else if (first_token.tag == .keyword_union) {
        @panic("todo");
    } else return null;
}

// ExprList <- (Expr COMMA)* Expr?
fn ExprList(src: [:0]const u8, start: usize, vm_opt: ?*Vm) error{Vm}!usize {
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
