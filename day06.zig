const std = @import("std");
const data = @embedFile("day06");

fn solve(n: usize) usize {
    var i: usize = 0;
    outer: while (i < data.len - n) : (i += 1) {
        var j: usize = 0;
        while (j < n) : (j += 1) {
            var k: usize = j + 1;
            while (k < n) : (k += 1) {
                if (data[i + j] == data[i + k]) continue :outer;
            }
        }
        return i + n;
    }

    return 0;
}

pub fn main() !void {
    std.debug.print("{}\n", .{solve(4)});
    std.debug.print("{}\n", .{solve(14)});
}
