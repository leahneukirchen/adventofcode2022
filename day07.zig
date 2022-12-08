const std = @import("std");
const data = @embedFile("day07");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = general_purpose_allocator.allocator();

const eql = std.mem.eql;

pub fn main() !void {
    defer std.debug.assert(!general_purpose_allocator.deinit());

    var lines = std.mem.tokenize(u8, data, "\n");

    var fs = std.StringHashMap(usize).init(gpa);
    defer {
        var keyiter = fs.keyIterator();
        while (keyiter.next()) |key| {
            gpa.free(key.*);
        }
        fs.deinit();
    }

    var cwd = std.ArrayList([]const u8).init(gpa);
    defer cwd.deinit();

    _ = lines.next();
    var root = try gpa.dupe(u8, "/");

    while (lines.next()) |line| {
        if (line.len == 0)
            break;

        if (eql(u8, line[0..4], "$ cd")) {
            var parts = std.mem.tokenize(u8, line, " ");
            _ = parts.next();
            _ = parts.next();
            var dir = parts.next().?;
            
            if (eql(u8, dir, "..")) {
                _ = cwd.pop();
            } else {
                try cwd.append(dir);
            }
        } else if (eql(u8, line, "$ ls")) {
            while (lines.peek()) |dirline| {
                if (dirline[0] == '$') break;
                _ = lines.next();
                
                if (eql(u8, dirline[0..4], "dir ")) continue;

                var parts = std.mem.split(u8, dirline, " ");

                var size = try std.fmt.parseInt(usize, parts.first(), 10);

                try fs.put(root, (fs.get(root) orelse 0) + size);

                var fullpath = std.ArrayList(u8).init(gpa);
                defer fullpath.deinit();
                for (cwd.items) |segment| {
                    try fullpath.append('/');
                    try fullpath.appendSlice(segment);

                    var s = try gpa.dupe(u8, fullpath.items);

                    var v = try fs.getOrPut(s);
                    if (!v.found_existing) {
                        v.value_ptr.* = size;
                    } else {
                        v.value_ptr.* += size;
                        gpa.free(s);
                    }
                }
            }
        }
    }

    var part1: usize = 0;
    var iter = fs.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.* <= 100_000)
            part1 += entry.value_ptr.*;
    }

    var needed: usize = 30000000 - (70000000 - fs.get(root).?);
    var part2: usize = 70000000;
    var iter2 = fs.iterator();
    while (iter2.next()) |entry| {
        if (entry.value_ptr.* > needed)
            part2 = std.math.min(part2, entry.value_ptr.*);
    }

    std.debug.print("{}\n{}\n", .{part1, part2});
}
