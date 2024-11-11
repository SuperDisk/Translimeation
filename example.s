	.arch armv4t
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 1
	.eabi_attribute 34, 0
	.eabi_attribute 18, 4
	.file	"example.c"
	.text
	.align	1
	.global	my_function
	.syntax unified
	.code	16
	.thumb_func
	.type	my_function, %function
my_function:
	@ Function supports interworking.
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	.syntax divided
@ 3 "example.c" 1
	LDR R1, =0x8793fe8
	CMP R0, R1
	BEQ no_jump
	LDR PC, =0x8098ac8
	no_jump:
	
@ 0 "" 2
	.thumb
	.syntax unified
	@ sp needed
	bx	lr
	.size	my_function, .-my_function
	.ident	"GCC: (15:12.2.rel1-1) 12.2.1 20221205"
