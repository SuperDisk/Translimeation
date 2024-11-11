// example.c
void my_function(void) {
  __asm(
        "LDR R1, =0x8793fe8\n\t"  // Load the required value to R1
        "CMP R0, R1\n\t"           // Compare R0 with R1
        "BEQ no_jump\n\t"          // If equal, branch to no_jump label
        "LDR PC, =0x8098ac8\n\t"   // Load the jump address to PC if R0 != R1, which effectively jumps to the address
        "no_jump:\n\t"             // Label where we do nothing and return normally if R0 == R1
        );
  return;
}
