#!/bin/bash

# arm-none-eabi-gcc -mthumb -S -march=armv4t -mtune=arm7tdmi -O1 example.c
# arm-none-eabi-ld -o dumbstub.elf -Ttext=[base_address] dumbstub.o

arm-none-eabi-as -o dumbstub.o dumbstub.s
arm-none-eabi-ld -o dumbstub.elf dumbstub.o
arm-none-eabi-objcopy -O binary dumbstub.elf dumbstub.bin
cat slime_original.gba dumbstub.bin > slime_dumb.gba
