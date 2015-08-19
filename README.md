tinymicro-mos6502-kansas-lava
=============================

A tiny microcomputer built around the MOS 6502 microprocessor.
It is inspired by the emulator at http://www.6502asm.com/.


Memory Layout
-------------

* `$0000 - $3FFF`: 16K of RAM
* `$0200 - $0600`: 1K video framebuffer
* `$E000 - $FFFF`: 8K of ROM


Video Output
------------

The 1K of video framebuffer is connected to a 800x600 VGA signal generator,
at a resolution of 32x32 in row-major order. Only the low 4 bits are used,
via the 6502asm.com palette.


Software Dependencies
---------------------

* Kansas Lava 0.2.4.1, from [my fork](http://github.com/gergoerdi/kansas-lava)
* [Shake build rules for Kansas Lava](http://github.com/gergoerdi/kansas-lava-shake)
* [MOS6502-kansas-lava](http://github.com/gergoerdi/mos6502-kansas-lava)
