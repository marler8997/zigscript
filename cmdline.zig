const builtin = @import("builtin");
const std = @import("std");

const zigscript = @import("zigscript.zig");
const interp = @import("interp.zig");

pub fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}
pub fn fatal(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.os.exit(0xff);
}

var windows_args_arena = if (builtin.os.tag == .windows)
    std.heap.ArenaAllocator.init(std.heap.page_allocator) else struct{}{};
pub fn cmdlineArgs() [][*:0]u8 {
    if (builtin.os.tag == .windows) {
        const slices = std.process.argsAlloc(windows_args_arena.allocator()) catch |err| switch (err) {
            error.OutOfMemory => oom(error.OutOfMemory),
            error.InvalidCmdLine => @panic("InvalidCmdLine"),
            error.Overflow => @panic("Overflow while parsing command line"),
        };
        const args = windows_args_arena.allocator().alloc([*:0]u8, slices.len - 1) catch |e| oom(e);
        for (slices[1..], 0..) |slice, i| {
            args[i] = slice.ptr;
        }
        return args;
    }
    return std.os.argv.ptr[1 .. std.os.argv.len];
}

pub fn main() !void {
    const args = blk: {
        const all_args = cmdlineArgs();
        var non_option_len: usize = 0;
        for (all_args) |arg_ptr| {
            const arg = std.mem.span(arg_ptr);
            if (!std.mem.startsWith(u8, arg, "-")) {
                all_args[non_option_len] = arg;
                non_option_len += 1;
            } else {
                fatal("unknown cmdline option '{s}'", .{arg});
            }
        }
        break :blk all_args[0 .. non_option_len];
    };

    if (args.len == 0) {
        try std.io.getStdErr().writer().writeAll("Usage: zigscript FILE\n");
        std.os.exit(0xff);
    }

    if (args.len != 1) @panic("script cmdline args not implemented");
    executeFile(std.mem.span(args[0]));
}

fn executeFile(filename: []const u8) void {
    var file = std.fs.cwd().openFile(filename, .{ }) catch |err| {
        std.log.err("open zigscript file '{s}' failed with {s}", .{filename, @errorName(err)});
        std.os.exit(0xff);
    };
    defer file.close();
    const script_mem = mapFileZ(file) catch |err| {
        std.log.err("mmap zigscript file '{s}' failed with {s}", .{filename, @errorName(err)});
        std.os.exit(0xff);
    };
    defer unmapFile(script_mem);

    // skip the shebang header
    const src = blk: {
        if (std.mem.startsWith(u8, script_mem, "#!")) {
            const next_line = if (std.mem.indexOf(u8, script_mem, "\n")) |nl| nl+1 else script_mem.len;
            break :blk script_mem[next_line.. :0];
        }
        break :blk script_mem;
    };
    executeSrc(filename, src);
}

fn executeSrc(filename: []const u8, src: [:0]const u8) void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){ };
    defer switch (gpa.deinit()) { .ok => {}, .leak => @panic("leak!") };
    var vm = zigscript.Vm{
        .src = src,
        .allocator = gpa.allocator()
    };
    defer vm.deinit();
    if (interp.Block(src, 0, &vm)) |end_opt| {
        const end = end_opt orelse fatal("failed to parse '{s}' as a Block", .{filename});

        const token = interp.lex(src, end);
        if (token.tag != .eof) fatal(
            "failed to fully parse '{s}' as a Block (file_len={}, parse_len={})",
            .{filename, src.len, end},
        );
        std.debug.assert(vm.scope_stack.items.len == 0);
    } else |vm_err| switch (vm_err) {
        error.Vm => {
            const err = vm.err orelse @panic("vm reported error but has none?");
            // TODO: print the token location etc
            const error_msg = err.getTestMsg();
            fatal("{s}: error: {s}", .{filename, error_msg});
        },
    }
}

fn mapFileZ(file: std.fs.File) ![:0]align(std.mem.page_size) u8 {
    if (builtin.os.tag == .windows) {
        // for now just read the file into memory, we can do memory mapping later
        return file.readToEndAllocOptions(
            std.heap.page_allocator,
            std.math.maxInt(usize),
            null,
            std.mem.page_size,
            0,
        );
    }
    const file_size = try file.getEndPos();
    const ptr = try std.os.mmap(
        null,
        file_size + 1,
        std.os.PROT.READ,
        std.os.MAP.PRIVATE,
        file.handle,
        0,
    );
    if (ptr[file_size] != 0) @panic("what to do here?");
    return ptr[0 .. file_size :0];
}
fn unmapFile(mem: []align(std.mem.page_size) u8) void {
    if (builtin.os.tag == .windows) {
        std.heap.page_allocator.free(mem);
    } else {
        std.os.munmap(mem);
    }
}
