const std = @import("std");
const data = @embedFile("day09");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = general_purpose_allocator.allocator();

const Pair = struct { x: i32, y: i32 };

fn signum(n: i32) i32 {
    if (n > 0) return 1;
    if (n < 0) return -1;
    return 0;
}

fn move_after(head: Pair, tail: Pair) Pair {
    if (std.math.absInt(head.x - tail.x) catch unreachable <= 1 and
        std.math.absInt(head.y - tail.y) catch unreachable <= 1)
        return tail;
    return .{ .x = tail.x + signum(head.x - tail.x),
              .y = tail.y + signum(head.y - tail.y) };
}

fn solve(comptime ropelen: usize) !usize {
    var lines = std.mem.tokenize(u8, data, "\n");

    var visited = std.AutoHashMap(Pair, void).init(gpa);
    defer visited.deinit();

    var rope = [_]Pair{.{.x = 0, .y = 0}} ** ropelen;

    try visited.put(rope[ropelen-1], {});

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var parts = std.mem.tokenize(u8, line, " ");
        var dir = parts.next().?[0];
        var amt = try std.fmt.parseInt(usize, parts.next().?, 10);

        var n: usize = 0;
        while (n < amt) : (n += 1) {
            switch (dir) {
                'L' => rope[0].x -= 1,
                'R' => rope[0].x += 1,
                'U' => rope[0].y -= 1,
                'D' => rope[0].y += 1,
                else => unreachable,
            }

            var i: usize = 1;
            while (i < ropelen) : (i += 1) {
                rope[i] = move_after(rope[i-1], rope[i]);
            }

            try visited.put(rope[ropelen-1], {});
        }
    }

    return visited.count();
}

pub fn main() !void {
    defer std.debug.assert(!general_purpose_allocator.deinit());

    std.debug.print("{}\n", .{try solve(2)});
    std.debug.print("{}\n", .{try solve(10)});
}
