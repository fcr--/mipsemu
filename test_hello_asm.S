#include <asm/regdef.h>
#include <asm/unistd.h>

	.text

	.globl __start
	.type __start, @function
	.set noreorder
__start:
	// call to ssize_t write(int fd, const void *buf, size_t count):
	li a0, 1
	lui a1, %hi($LC0)
	addiu a1, a1, %lo($LC0)
	li a2, 1
	li v0, __NR_writev
	syscall

	// call to exit_group(int status):
	move a0, zero
	li v0, __NR_exit_group
	syscall

	.align 0

$LC0:
	.word $LC1
	.word $LC1end - $LC1
$LC1:
	.ascii "Hola mundo!\n"
$LC1end:
	.ascii ""
	.align 2

// vi: noet
