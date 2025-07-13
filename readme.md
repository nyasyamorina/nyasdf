# nyasdf

*nyas Data Fomat*

## Basic

`nyasdf` provides simple and easy to use APIs to save and load arbitrary data.

The fondamental type is `Data` union, it can contain value data types or ref data types, which can be combined to form complex data structures.

### Value Types

These types hold... well... value, currently they are:

- `Data.Null`

- `Data.Bool`

- `Data.Byte`

- `Data.F16`

- `Data.F32`

- `Data.F64`

- `Data.Bytes` holds a array slice of byte,

- `Data.String` also holds a array slice of byte, but it holds `[:0]const u8` instead of `[]const u8`, that means it is compatible with C string,

- `Data.Int` holds arbitrary long integer upto `i65535`/`u65535` (zig limitation), use `set(int: anytype) void` to set, and use `get(comptime Int: type) Int` to get.

### Ref Types

These types hold references (pointers) to other `Data`, they are the key components in describing data structure, if you think of `Data` as nodes and the data structure as a graph, then ref types descibe the node connection in the graph.

- `Data.Pair` holds two data references,

- `Data.List` holds a array of data reference.

### Example

A json

```json
{
    "name": "Alice",
    "age": 16,
    "interest": [
        "gaming",
        "movie",
    ],
}
```

can be represented using `nyasdf.Data`:

```zig
const Data = @import("nyasdf").Data;

const name_key: Data = .{.string = .{.val = "name"}};
const name: Data = .{.string = .{.val = "Alice"}};
const name_pair: Data = .{.pair = .{.@"0" = &name_key, .@"1" = &name}};

const age_key: Data = .{.string = .{.val = "age"}};
var age: Data = .{.int = .init(allocator)};
try age.int.set(@as(u8, 16));
const age_pair: Data = .{.pair = .{.@"0" = &age_key, .@"1" = &age}};

const interest1: Data = .{.string = .{.val = "gaming"}};
const interest2: Data = .{.string = .{.val = "movie"}};
var interests = [_]*const Data {&interest1, &interest2};
const interest_list: Data = .{.list = .{.val = .fromOwnedSlice(&interests)}};

const interest_key: Data = .{.string = .{.val = "interest"}};
const interest_pair: Data = .{.pair = .{.@"0" = &interest_key, .@"1" = &interest_list}};

var pairs = [_]*const Data {&name_pair, &age_pair, &interest_pair};
const json: Data = .{.list = .{.val = .fromOwnedSlice(&pairs)}};
```

## Reducer

`nyasdf` provides a powerful tool called `Reducer` to reduce duplicate data, this is very useful for compressing object that contains tons of duliated data, such as json.

For a given array slice of data `data_slice: []*Data`, reducion can be done by using:

```zig
const kepping_count = try Reducer.reduceDirect(allocator, data_slice, null);
```

The elements in the data slice are be rearranged, all non-duplicated data are moved to the beginning of the slice, the return value `keeping_count` is the number of non-duplicated data, and all non-duplicated ref data are changed to point to non-duplicated data. Therefore, the remaining part `data_slice[keeping_count..]` can be safely destroyed.

## WriteIterator

`nyasdf` provides a generic writer `WriteIterator` and a file-specified type called `FileWriteIterator`.

`WriteIterator` takes a array slice of data `data_slice: []const *const Data` and writes all data and extra info into file:

```zig
_ = try FileWriteIterator.writeDirect(allocator, file, data_slice);
```

But why `WriteIterator` not `Writer` ?

Because `WriteIterator` provides APIs that allow fine control, maybe you want to write some message between data? Then you can write your code like this:

```zig
var w_iter: FileWriteIterator = .init(allocator, file);
defer w_iter.deinit();
try w_iter.setDataSlice(data_slice);

try w_iter.internalWriter().print("writing data starts", .{});
var index: usize = 0;
while (try w_iter.writeNext()) |_| : (index += 1) {
    try w_iter.internalWriter().print("data {d} has been written", .{index});
}
_ = try w_iter.writePosTable();
_ = try w_iter.writeEntry();
try w_iter.internalWriter().print("writing data ends", .{});
```

**Important**: make sure ref data are all pointing into the given datas lice, `WriteIterator` cannot get data outside the slice.

## ReadIterator

Like `WriteIterator`, `nyasdf` also provides `ReadIterator` and `FileReadIterator`.

Read nyasdf using `ReadIterator` will result a `DataPackage`, here is how to use it:

```zig
const pack = try nyasdf.FileReadIterator.readDirect(allocator, file);
defer pack.deinit();
```

and accessing `Data` using `pack.list.items[...]`.

Although `ReadIterator` also has fine control APIs likes `WriteIterator`, but the use case is very unclear, so not recommand to use it.

## convert

This is the submodule `nyasdf.convert`

### fromJsonValue

Collect json values to `DataPackage`:

```zig
const json_value: std.json.Value = ...;

const pack = try nyasdf.convert.fromJsonValue(allocator, json_value);
defer pack.deinit();
```

Note that json values with `.number_string` tag will convert to `Data.String`, see `std.json.ParseOptions.parse_numbers` for more infomation.

----

### TODO

- convert json to nyasdf (add func take `std.json.Scanner` or `std.json.Reader` and build `DataPackage` without using `std.json.Value`), and back (? convert nyasdf back to json need to check loop references, and I don't know how to do it properly)

- convert zig types to nyasdf (very useful for small object, but it make no sense for large object, even for `std.ArrayList`)

- `DataPackage` should use `ArenaAllocator` to manage memory to allow not copy string strategy in `serialize.fromJsonValue`
