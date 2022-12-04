const std = @import("std");
const data = @embedFile("day03");

fn priority(c: u8) i32 {
    return switch (c) {
        'a'...'z' => c - 'a' + 1,
        'A'...'Z' => c - 'A' + 1 + 26,
        else => unreachable,
    };
}

pub fn main() !void {
    var part1: i32 = 0;
    var part2: i32 = 0;

    var lines = std.mem.split(u8, data, "\n");

    part1: while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var mid: usize = line.len / 2;
        for (line[0..mid]) |c1| {
            for (line[mid..line.len]) |c2| {
                if (c1 == c2) {
                    part1 += priority(c1);
                    continue :part1;
                }
            }
        }
    }

    lines = std.mem.split(u8, data, "\n");

    part2: while (lines.next()) |line1| {
        if (line1.len == 0)
            break;
        var line2 = lines.next().?;
        var line3 = lines.next().?;

        for (line1) |c1| {
            for (line2) |c2| {
                if (c1 == c2) {
                    for (line3) |c3| {
                        if (c1 == c3) {
                            part2 += priority(c1);
                            continue :part2;
                        }
                    }
                }
            }
        }
    }

    std.debug.print("{} {}\n", .{ part1, part2 });
}
