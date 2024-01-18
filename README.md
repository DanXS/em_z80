# EM Z80

This project aims to implement an emulator and dissabler for the z80 CPU.

It is implemented in Rust, my first go at using Rust so I'm sure I may not be using best practices.

There are quite a few static arrays which aren't safe and so I've used unsafe blocks so that Rust compiles them.

Probably I should be using dynamica arrays and the Box approach but I may want to generate a version targeted to embeded devices so I'm trying to avoid too much dynamic memory allocation.

On a real system there is only one set of ram and rom chips which are shared so I'm sticking with that
approach for now.

I've added a mutex around the ram so that should I want to emulate a particular computer based around the z80, for example the ZX Spectrum, then other periferals such as the display can share the same memory on a different thread.

I use large look up tables derived from [link](https://clrhome.org/table/) which lists all the instructions and how by opcode.  The z80 has quite a large set of instructions or variations on a particular instruction so there are multiple tables for instructions encoded with multiple bytes.

This is probably a waste of memory when the instructions could be decoded directly from the bit patterns in the various opcodes, but it makes disassbly easier and allows the table to at least point to the relivant instruction class.  The specifics of the instruction are still decoded by looking at bit patterns describing which registers are used etc.  I followed this document [link][def] as a reference describing the instructions and how each instruction affects the flags.

The intention is to keep this part purely for the z80 chip emulation and disassbly without any UI or dependencies an particular framework or operating system.

Later I may want to build UI's for web assembly or native window based applications for various platforms, or perhaps to work on embeded versions.  Therefore I'll probably adopt this part to be a sub-module or library which can be used elsewhere.

[def]: https://www.zilog.com/docs/z80/um0080.pdf