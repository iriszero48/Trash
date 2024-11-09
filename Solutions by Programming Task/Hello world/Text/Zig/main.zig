pub fn main() !void {
    try @import("std").io.getStdOut().writer().print("{s}\n", .{"Hello world!"});
}
