const std = @import("std");


pub const ReadExactError = error { InsufficientRead } || std.fs.File.ReadError;

pub fn readExact(file: std.fs.File, buff: []u8) ReadExactError!void {
    if (try file.read(buff) != buff.len) {
        return ReadExactError.InsufficientRead;
    }
}

pub const SignedIntLength = packed struct(u8) {
    length: u7,
    negative: bool,
};
