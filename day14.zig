// needs -O ReleaseFast, else super slow

const std = @import("std");
const data = @embedFile("day14");

var area = [_][800]u8{ [_]u8{0} ** 800 } ** 800;

fn draw(ox: usize, oy: usize, x: usize, y: usize) void {
    if (ox == x) {
        var vy = std.math.min(oy, y);
        while (vy <= std.math.max(oy, y)) : (vy += 1)
            area[x][vy] = 9;
    } else {
        var vx = std.math.min(ox, x);
        while (vx <= std.math.max(ox, x)) : (vx += 1)
            area[vx][y] = 9;
    }
}

fn drop_sand(y1: usize, x1: usize) bool {
    var y = y1;
    var x = x1;

    while (x < 799 and y < 799) {
        if (area[y+1][x] == 0) {
            y += 1;
            continue;
        } 
        if (area[y+1][x-1] == 0) {
            x -= 1;
            y += 1;
            continue;
        }
        if (area[y+1][x+1] == 0) {
            x += 1;
            y += 1;
            continue;
        }
        area[y][x] = 1;
        return true;
    }
    
    return false;
}

pub fn main() !void {
    var lines = std.mem.tokenize(u8, data, "\n");

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        var coords = std.mem.tokenize(u8, line, " -> ");

        var ox: usize = 0;
        var oy: usize = 0;
        while (coords.next()) |coord| {
            var ns = std.mem.tokenize(u8, coord, ",");
            var y = try std.fmt.parseInt(usize, ns.next().?, 10);
            var x = try std.fmt.parseInt(usize, ns.next().?, 10);

            if (ox != 0)
                draw(ox, oy, x, y);

            ox = x;
            oy = y;
        }
    }


    var part1: usize = 0;
    while (drop_sand(0, 500))
        part1 += 1;

    for (area) |*line| {
        for (line) |*item| {
            if (item.* == 1)
                item.* = 0;
        }
    }

    var bottom: usize = 799;
    loop: while (bottom > 0) : (bottom -= 1) {
        for (area[bottom]) |item| {
            if (item == 9) {
                draw(bottom + 2, 0, bottom + 2, 799);
                break :loop;
            }
        }
    }
    
    var part2: usize = 0;
    loop: while (drop_sand(0, 500)) {
        part2 += 1;

        if (area[0][500] != 0)
            break :loop;
    }

//    for (area) |line| {
//        for (line) |item| {
//            std.debug.print("{}", .{item});
//        }
//        std.debug.print("\n", .{});
//    }

    std.debug.print("{}\n{}\n", .{part1, part2});
}
