const std = @import("std");
const data = @embedFile("day05");

const Stack = std.BoundedArray(u8, 64);

pub fn main() !void {
    var lines = std.mem.split(u8, data, "\n");

    var stacks = [_]Stack{try Stack.init(0)} ** 9;

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        for (stacks) |*stack, j| {
            var c = line[1 + (j * 4)];
            if (c != ' ') {
                try stack.append(c);
            }
        }
    }

    for (stacks) |*stack| {
        std.mem.reverse(u8, stack.slice());
    }

    var stacks2 = stacks;

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var parts = std.mem.tokenize(u8, line, " ");
        _ = parts.next();
        var n = try std.fmt.parseInt(usize, parts.next().?, 10);
        _ = parts.next();
        var from = try std.fmt.parseInt(usize, parts.next().?, 10) - 1;
        _ = parts.next();
        var to = try std.fmt.parseInt(usize, parts.next().?, 10) - 1;

        var j: usize = 0;
        while (j < n) : (j += 1) {
            try stacks[to].append(stacks[from].pop());
        }

        var f = stacks2[from].slice();
        try stacks2[to].appendSlice(f[f.len - n .. f.len]);
        try stacks2[from].resize(f.len - n);
    }

    for (stacks) |*stack| {
        std.debug.print("{c}", .{stack.pop()});
    }
    std.debug.print("\n", .{});

    for (stacks2) |*stack| {
        std.debug.print("{c}", .{stack.pop()});
    }
    std.debug.print("\n", .{});
}
