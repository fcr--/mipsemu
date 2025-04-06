# mipsemu
MIPS Emulator written in AWK

Status: DRAFT.

## Introduction
A very slow (and potentially very buggy) emulator for running statically linked programs compiled for Linux MIPS32 LittleEndian (also known as simply `mipsel`).  You can use any toolchain that targets some kind of mipsel linux, using either glibc or musl (or none if you dare to call syscalls directly).

Speaking of syscalls only a very little subset is implemented, which eventually will grow.

Same for instructions, only a strict subset of the official MIPS32 instructions are implemented, and once again, this set will grow as more examples are added, but be aware that will be tons of bugs in their implementation.

If you really need an emulator, use `qemu-mipsel` instead instead of this script.  I'm only writing this because I can.

## Running the examples

```
$ LC_ALL=C awk -vhex=test_basic1_nostdlib.hex -fmipsemu.awk -vdebug=instr,write_u32; echo $?
[..]
42
$ LC_ALL=C awk -vhex=test_hello.hex -fmipsemu.awk
hola mundo
$ LC_ALL=C awk -vhex=test_hello_printf.hex -fmipsemu.awk
hola mundo
$ LC_ALL=C awk -vhex=test_calc.hex -fmipsemu.awk -vargs=1,18,sub,3,2,mul,1,index,1,index,mod,exch,dup,3,index,exch,idiv,mul,add
stack: -17,-17
$ make tests
[..] (hopefully a ton of "✅ ok" lines)
```

## TODO

* [ ] Remove literal hex constants from mipsemu.awk (gawk extension)
* [ ] Implement examples that use argc & argv, and gradually increase the supported instruction subset:
  * [ ] add a 32bit signed & unsigned calculator.
  * [ ] add a 64bit calculator (same as before).
  * [ ] introduce soft-fp example.
* [ ] Write an example program that mallocs memory!!! (this will require extra syscalls)
* [ ] Add an example that reads files. (extra syscalls again)

## Reference material
* Intro into MIPS assembly: https://courses.cs.washington.edu/courses/cse378/10sp/lectures/lec05-new.pdf
* MIPS® Architecture for Programmers, Volume II-A: The MIPS32® Instruction Set Manual: https://hades.mech.northwestern.edu/images/1/16/MIPS32_Architecture_Volume_II-A_Instruction_Set.pdf
* SysV MIPS ABI: https://refspecs.linuxfoundation.org/elf/mipsabi.pdf
* syscalls:
  * general stuff: https://github.com/torvalds/linux/blob/master/arch/mips/include/uapi/asm/unistd.h
  * signatures: https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/ioctl.h
  * list: https://github.com/draperlaboratory/hope-qemu/blob/master/linux-headers/asm-mips/unistd_o32.h
