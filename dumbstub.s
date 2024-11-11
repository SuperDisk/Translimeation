.section .text
.global _start
.syntax unified
.thumb

_start:
    // [1] Save low registers and LR
    PUSH {R0-R7}

    // [2] Check if R0 == 0x8793EF0, if true, restore registers and return
    LDR R2, =0x8793EF0   // Load the comparison value into R2
    CMP R0, R2           // Compare R0 with 0x8793EF0
    BEQ restore_and_return // If equal, branch to restore_and_return

    // [3] Restore low registers and jump to 0x8098AC8
    POP {R0-R7}      // Restore low registers and LR
    LDR R2, =0x8098AC8
    bx r2

restore_and_return:
    // Restore low registers and return
    POP {R0-R7}
    BX LR
