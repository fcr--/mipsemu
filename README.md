# mipsemu
MIPS Emulator written in AWK

Status: DRAFT.

## Introduction
A very slow (and potentially very buggy) emulator for running statically linked programs compiled for Linux MIPS32 LittleEndian (also known as simply `mipsel`).  You can use any toolchain that targets some kind of mipsel linux, using either glibc or musl (or none if you dare to call syscalls directly).

Speaking of syscalls only a very little subset is implemented, which eventually will grow.

Same for instructions, only a strict subset of the official MIPS32 instructions are implemented, and once again, this set will grow as more examples are added.

If you really need an emulator use `qemu-mipsel` instead instead of this script.  I'm only writing this because I can.

## Running the examples:

```
$ awk -vhex=test_basic1_nostdlib.hex -fmipsemu.awk -vdebug=instr,write_u32; echo $?
[..]
42
$ awk -vhex=test_hello.hex -fmipsemu.awk
hola mundo
$ awk -vhex=test_hello_printf.hex -fmipsemu.awk
hola mundo
```

## Reference material:
* Intro into MIPS assembly: https://courses.cs.washington.edu/courses/cse378/10sp/lectures/lec05-new.pdf
* MIPS® Architecture for Programmers, Volume II-A: The MIPS32® Instruction Set Manual: https://hades.mech.northwestern.edu/images/1/16/MIPS32_Architecture_Volume_II-A_Instruction_Set.pdf
* SysV MIPS ABI: https://refspecs.linuxfoundation.org/elf/mipsabi.pdf
* syscalls:
  * general stuff: https://github.com/torvalds/linux/blob/master/arch/mips/include/uapi/asm/unistd.h
  * signatures: https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/ioctl.h
  * list: https://github.com/draperlaboratory/hope-qemu/blob/master/linux-headers/asm-mips/unistd_o32.h
