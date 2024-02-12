# EM Z80

This project aims to implement an emulator and dissabler for the z80 CPU.

It is implemented in Rust, my first go at using Rust so I'm sure I may not be using best practices.

There are quite a few static arrays which aren't safe and so I've used unsafe blocks so that Rust compiles them.

Probably I should be using dynamica arrays and the Box approach but I may want to generate a version targeted to embeded devices so I'm trying to avoid too much dynamic memory allocation.

On a real system there is only one set of ram and rom chips which are shared so I'm sticking with that
approach for now.

I use large look up tables derived from [https://clrhome.org/table](https://clrhome.org/table/) which lists all the instructions and how by opcode.  The z80 has quite a large set of instructions or variations on a particular instruction so there are multiple tables for instructions encoded with multiple bytes.

This is probably a waste of memory when the instructions could be decoded directly from the bit patterns in the various opcodes, but it makes disassbly easier and allows the table to at least point to the relivant instruction class.  The specifics of the instruction are still decoded by looking at bit patterns describing which registers are used etc.  I followed this document [https://www.zilog.com/docs/z80/um0080.pdf](https://www.zilog.com/docs/z80/um0080.pdf) as a reference describing the instructions and how each instruction affects the flags.

The intention is to keep this part purely for the z80 chip emulation and disassbly without any UI or dependencies an particular framework or operating system.

I'm using slint for the desktop UI which I may also use for a web based version using web assembly which is also supported by slint: [https://slint.dev/releases/1.2.0/docs/slint/](https://slint.dev/releases/1.2.0/docs/slint/)

I plan on building an embedded version as well probably without the debugger, just the emulator.  I found a very cheap ESP based board which has old school PS/2 connections for keyboard and mouse and had a VGA output for a monitor.  So far i've only seen it used with arduino/cpp based projects, so it may take some work to get it running with rust: [https://www.lilygo.cc/products/fabgl-vga32](https://www.lilygo.cc/products/fabgl-vga32)



