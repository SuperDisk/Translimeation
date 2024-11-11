#!/bin/bash

arm-none-eabi-gcc -mthumb -S -march=armv4t -mtune=arm7tdmi -O1 example.c
arm-none-eabi-as -o example.o example.s
# arm-none-eabi-ld -o example.elf -Ttext=[base_address] example.o
arm-none-eabi-ld -o example.elf example.o
arm-none-eabi-objcopy -O binary example.elf example.bin
