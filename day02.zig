// hat tip to https://gitlab.cs.washington.edu/fidelp/advent-of-code-2022/-/blob/main/advent-2022-02-rough.fs

const std = @import("std");
const data = @embedFile("day02");

pub fn main() !void {
    var lines = std.mem.split(u8, data, "\n");

    var part1: i32 = 0;
    var part2: i32 = 0;

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var p1: i32 = line[0] - 'A';
        var p2: i32 = line[2] - 'X';

        part1 += 3 * @rem(p2 - p1 + 4, 3) + (p2 + 1);

        p2 = @rem(p1 + p2 + 2, 3);

        part2 += 3 * @rem(p2 - p1 + 4, 3) + (p2 + 1);
    }

    std.debug.print("{} {}\n", .{ part1, part2 });
}
