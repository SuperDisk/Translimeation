// Shared font selection. Context bytes 106..107 are padding in the stock struct.
// Plain strings relocated to expanded ROM opt into the DS font; RAM-built
// counters and original ROM strings retain stock metrics.
.syntax unified
.cpu arm7tdmi
.thumb
.global font_select, font_spacing, plain_start
font_select:
    push {lr}
    bl font_table
    lsls r0, r0, #16
    lsrs r0, r0, #13
    adds r0, r0, r1
    pop {r1}
    bx r1
font_spacing:
    push {lr}
    bl font_table
    mov r0, r10
    lsls r0, r0, #3
    ldr r0, [r1, r0]
    lsrs r0, r0, #23
    cmp r0, #17
    blo stock_spacing
    movs r2, #0
stock_spacing:
    pop {r1}
    cmp r2, #0
    beq no_spacing
    bx r1
no_spacing:
    ldr r0, spacing_resume
    bx r0
font_table:
    movs r1, r6
    lsrs r3, r1, #24
    cmp r3, #2
    beq context_ready
    cmp r3, #3
    beq context_ready
    movs r1, r5 // Centered glyph caller holds its context in r5.
context_ready:
    ldr r3, dialogue_context
    cmp r1, r3
    beq imported
    adds r1, #255
    ldrb r1, [r1, #7]
    cmp r1, #1
    beq imported
    ldr r1, stock_table
    bx lr
imported:
    ldr r1, imported_table
    bx lr
plain_start:
    push {r4, r5, r6, r7, lr}
    sub sp, #0x108
    movs r4, r1
    lsls r2, r2, #24
    lsrs r3, r1, #23
    cmp r3, #17
    beq relocated
    movs r3, #0
    b set_flag
relocated:
    movs r3, #1
set_flag:
    add r1, sp, #0x104
    strb r3, [r1, #2]
    ldr r3, plain_resume
    bx r3
.balign 4
spacing_resume: .word 0x08096b29
plain_resume: .word 0x08096c49
dialogue_context: .word 0x02001080
imported_table: .word 0x11111110
stock_table: .word 0x22222220
