// 01 FE is a reading pause, outside the original 0010..01B8 glyph range.
// Enter through the extended-glyph dispatch slot. Original glyphs fall through.
// Pending state 6 is unused by the stock DELAY command (which never sets pending).
.syntax unified
.cpu arm7tdmi
.thumb
.global reading_start, reading_continue
reading_start:
    ldr r4, context
    movs r0, #0x86
    lsls r0, r0, #1
    adds r2, r4, r0
    ldr r0, [r2]
    ldrb r1, [r0]
    cmp r1, #0xfe
    beq start_pause
    movs r3, #1
    ldr r0, original_extended
    bx r0
start_pause:
    adds r0, #1
    str r0, [r2]
    movs r0, #0x93
    lsls r0, r0, #1
    adds r1, r4, r0
    ldrb r0, [r1]
    movs r2, #4
    orrs r0, r2
    strb r0, [r1]
    adds r1, #0xf
    movs r0, #6
    strb r0, [r1]
    bl reset_speed
    movs r2, #1
    bl prompt
    b finish
.align 2
reading_continue:
    ldr r4, context
    bl reset_speed
    ldr r0, keys
    ldrh r0, [r0]
    movs r1, #0xf1
    tst r0, r1
    beq finish
    // Deliberately omit the actor-busy gate and 030044B8 acknowledgement.
    // Actors may be waiting for a later CUE while the reader turns this page.
    movs r0, #0xa6
    ldr r5, sound
    bl call_r5
    movs r2, #0
    bl prompt
    movs r0, #0x93
    lsls r0, r0, #1
    adds r1, r4, r0
    ldrb r0, [r1]
    movs r2, #4
    bics r0, r2
    strb r0, [r1]
finish:
    // Return through the interpreter's original epilogue, not its fast loop.
    ldr r0, interpreter_return
    bx r0
reset_speed:
    movs r0, #0x92
    lsls r0, r0, #1
    adds r1, r4, r0
    movs r0, #0
    strh r0, [r1]
    strb r0, [r1, #4]
    strb r0, [r1, #12]
    bx lr
prompt:
    push {lr}
    ldr r1, window_x
    adds r1, r4, r1
    ldrb r0, [r1]
    movs r3, #6
    ldrsb r3, [r1, r3]
    ldrb r1, [r1, #1]
    ldr r5, draw_prompt
    bl call_r5
    pop {r0}
    bx r0
call_r5:
    bx r5
.align 2
context: .word 0x02001080
keys: .word 0x03004054
window_x: .word 0x129
original_extended: .word 0x08096271
interpreter_return: .word 0x08096499
sound: .word 0x08002bad
draw_prompt: .word 0x08097cf1
