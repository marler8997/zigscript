# zigscript

A companion scripting language for Zig developers. The driving goals are to:

1) be as easy/fast to write as python
2) be seamless for Zig developers to write

ZigScript is meant to exist in a world with Zig.  It's designed to accomodate the use
cases that Zig isn't optimal for and leaves the everything else to Zig.

ZigScript uses the Zig tokenizer along with a modified version of its grammar.

Like Zig's comptime, ZigScript will automatically manage memory.  Users can create objects,
append to lists and perform any other operation that requires memory allocation without
managing it themselves.

ZigScript currently doesn't support explicit types. The reason is that if it's worth your time
to specify explicit types, then it's probably worth your time to write your program in Zig instead.

ZigScript doesn't use fixed-width integer types, instead, all numbers are represented by `std.math.big.int`.
Support for floats is not yet here, but they'll likely be an extension of big integers where the decimal
position is tracked with an additional field.

## Ideas

* Interoperability

It should be easy for ZigScript to interoperate/leverage Zig modules.  I'm not sure of the
best approach for this yet.

* Interpolated strings?

```
const x = 0;
@out(i"x is {x}");
```
