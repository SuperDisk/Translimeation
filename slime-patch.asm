.gba
.open "slime_original.gba","output.gba",0x08000000

Decompress EQU 0x8098ac8

//==================CODE============================
.org 0x087d1c70
.area 0xDAC
.align 4

MaybeDecompress:
  push {r0,r1}
  ldr     r1, =0x08800000
  cmp     r0, r1
  blt     less_than

  ;; my stuf
  pop {r4,r5,r6,r7,lr}
  bx lr

less_than:
  pop {r0,r1}
  mov r4, r8
  mov r5, r9
  mov r6, r10
  mov r7, r11
  push {r4,r5,r6,r7}
  ldr r7, =0x02010880
  ldmia r0!,{r4}

  push r0
  ldr r0, =jumpBack|1
  mov lr, r0
  pop r0
  bx lr

.pool
.dw jumpBack
.dw 0x2010880

.endarea

//====================HOOKS==========================
.org Decompress
droutine:
  push {r4,r5,r6,r7,lr}
  ldr r4, =MaybeDecompress|1
  bx r4

.pool
.dw MaybeDecompress

jumpBack:

//Close file and finish
.close
