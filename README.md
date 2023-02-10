# TMON
TMON - a serial terminal and monitor for the SC-1 and TEC-1F computers

This program is a complete serial port based monitor for the SC-1 and TEC-1F series of Z-80 computers. TMON uses the 'bit banged' serial port built into the machine, in conjunction with the SCMON Monitor ROM. The program runs without alteration on either platform as it uses the SCMON System Calls library to communicate with the hardware of either machine independently.

Note: This code requires the SCMON Monitor, version 1.8 or higher, available from https://github.com/crsjones/Southern-Cross-Computer-z80/tree/main/SouthernCrossSBC_Monitor


## Requirements

A Southern Cross SC-1 computer, or, a TEC-1F computer
The TEC-1F must be set up for 8k addresing and requires 8k of RAM
SCMON version 1.8 or newer, compiled for your machine type
Bit-banged serial interface to a PC or serial terminal.
 - The Serial port is configured with the standard SCMON settings - 4800bps,n,8,2 based on a 4MHz clock
 - Terminal Emulation type VT100 is required (most serial terminal program e.g. PuTTY support VT100 by default)

## How to use TMON

TMON is an interactive tool, that works with a serial terminal e.g. PuTTY or Tera Term on an PC, or a 'real' VT100 serial terminal such as a Wyse WY-60. The SC/TEC keypad and 7-seg displays are not used once the program starts, and do not do anything.

### Starting up TMON

Transfter TMON to the SC/TEC by yuour favourite means - I use Intel HEX transfer; it takes about 20 seconds to transfer the nearly 4k of HEX code. Once you have TMON loaded onto the SC/TEC, run it in the usual fashion and look at your serial terminal

```
Compiled Platform: TEC-1F
TMON Version 0.2b
MONitor ROM version: 1.8
RAM Found between 2000h and 5FFFh - 4000h bytes
2000 >
```

The above is the default display you should see from TMON (Obviously if run on an SC-1 the messages change accordingly). TMON is now awaiting your input and you can type commands from the Availabe Commands list, below.

The command 'prompt' is the final line:

````
2000 > 
````

The 2000 represents the CURRENT ADDRESS in HEX. Many commands default to their actions interacting with memory at this address. The CURRENT ADDRESS changes as you interact with TMON e.g. inputting code and data, and can be set by the ADDR command.



## Available Commands

### help commands

EXIT  - Quites to SCMON
HELP  - Help information
? - List of commands
LIST - alias for HELP
CLS - clears the screen

### file transfer commands

INTEL - tranfer an Intel HEX file from PC to SC/TEC

## programming related commands

Note - the xxxx represents a memory address input in hexadecimal. xxxx is optional; if specified it will override but not alter the 'current address' pointer. If not specified, the command acts based on the 'current address'.

ADDR xxxx - Set the CURRENT ADDRESS. If no address supplied, display the CURRENT ADDRESS instead.
GO xxxx - Execute code. The is the equivalent of pressing GO on a classic TEC or Fn 0 on an SC.
DUMP xxxx - DUMP the contents of 64 bytes of memory; provides HEX and aSCII outputs so memory can be examined
DIS xxxx - Disasemble Z80 instructions. Provides a disassembly of the opcodes found in memory, 16 opcodes at a time
DATA xxxx - Interactively Input data into memory. Input one hex byte at a time; the value input is caved to the CURRENT ADDDRESS. Enter Q to quit input mode.

## testing & informational commands

BEEP  - makes a beep sound on the SC/TEC
VER - Displays the TMON and SCMON versions
HARDWARE - Returns the machine type - SC-1 or TEC-1F
RAMCHK - Runs a test to determine the size and location of any RAM within the Z80's 64k address space. Will pick up multiple RAM blocks even if not contiguous.

