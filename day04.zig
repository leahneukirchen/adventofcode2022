const std = @import("std");
const data = @embedFile("day04");

pub fn main() !void {
    var part1: i32 = 0;
    var part2: i32 = 0;

    var lines = std.mem.split(u8, data, "\n");

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var parts = std.mem.tokenize(u8, line, ",-");

        var a = try std.fmt.parseInt(i32, parts.next().?, 10);
        var b = try std.fmt.parseInt(i32, parts.next().?, 10);
        var c = try std.fmt.parseInt(i32, parts.next().?, 10);
        var d = try std.fmt.parseInt(i32, parts.next().?, 10);

        if ((c - a) * (d - b) <= 0) part1 += 1;
        if ((d - a) * (c - b) <= 0) part2 += 1;
    }

    std.debug.print("{} {}\n", .{ part1, part2 });
}
