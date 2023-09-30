# zigscript

A companion scripting language for Zig developers.

This is an exploration on a scripting language whose goals are to:

1) be as easy/fast to write as python
2) be seamless for Zig developers to write

I'm starting out this experiment by first implementing an interpreter for Zig based on
the Zig grammar.  With this in place, I'll be looking into ways I can enhance the
syntax to take advantage of new "script-like" features such as automatic memory
management and builtin data structures like map/list.

## Thoughts

ZigScript is meant to exist in a world with Zig.  This means that ZigScript doesn't need
to be designed for use cases that Zig would already be a good fit for.

For this reason, maybe ZigScript shouldn't support explicit types?  The idea here is that
if it's worth your time to annotate your program with types, maybe you should be using
Zig instead?
