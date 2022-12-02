const std = @import("std");
const data = @embedFile("day01");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = general_purpose_allocator.allocator();

fn moreThan(context: void, a: i32, b: i32) std.math.Order {
    _ = context;
    return std.math.order(b, a);
}

pub fn main() !void {
    var pq = std.PriorityQueue(i32, void, moreThan).init(gpa, {});

    var lines = std.mem.split(u8, data, "\n");

    var food: i32 = 0;
    while (lines.next()) |line| {
        var x: i32 = std.fmt.parseInt(i32, line, 10) catch {
            try pq.add(food);
            food = 0;
            continue;
        };
        food += x;
    }

    var max3food: i32 = 0;
    max3food += pq.remove();
    std.debug.print("{}\n", .{max3food});
    max3food += pq.remove();
    max3food += pq.remove();
    std.debug.print("{}\n", .{max3food});
}
