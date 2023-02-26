# TMON
TMON (short for Terminal MONitor) is a complete serial port based monitor for the SC-1 and TEC-1F series of Z-80 computers. TMON uses the 'bit banged' serial port that is built into the machine, in conjunction with the SCMON Monitor ROM.

The program runs without alteration on either the Talking Electronics TEC-1F or the Southern Cross SC-1 hardware, as it uses the SCMON System Calls library to communicate with the hardware of either machine.

Note: This code requires the SCMON Monitor, version 1.8 or higher, available from https://github.com/crsjones/Southern-Cross-Computer-z80/tree/main/SouthernCrossSBC_Monitor be sure to use the correct version for your hardware.

## Requirements

- A Southern Cross SC-1 computer, or, a TEC-1F computer
- The TEC-1F must be set up for 8k addresing; TMON requires roughly 4k of RAM
- SCMON version 1.8 or newer
- Bit-banged serial interface connected to a PC or serial terminal
- The Serial port is configured with the standard SCMON settings - 4800bps,N,8,2 - based on a 4MHz Z-80 clock
- Terminal Emulation type VT100 is required (most serial terminal programs e.g. PuTTY support VT100 by default)

## How to use TMON

TMON is an interactive tool, that works with a serial terminal e.g. PuTTY or Tera Term on an PC, or a 'real' VT100 serial terminal such as a Wyse WY-60. The SC/TEC keypad and 7-seg displays are not used once the program starts, and do not do anything.

Interactions with TMON are via the serial console. The user types commands and the results are displayed on the terminal.

### Starting up TMON

Transfer TMON to the SC/TEC by your favourite means - I use SCMON's built in Intel HEX transfer; it takes about 20 seconds to transfer the nearly 4k of HEX code. TMON loads into memory from address 2000h. Once you have TMON loaded onto the SC/TEC, run it in the usual fashion and look at your serial terminal.

```
TMON Version 0.4b
SCMON version: 1.8
Compiled Platform: TEC-1F, Hardware keyboard
RAM Found between 2000h and 5FFFh - 16384 bytes
2000 >
```

## Using TMON

The above text is the default display you should see from TMON upon startup. (Obviously if run on an SC-1 the messages change accordingly). TMON is now awaiting your input and you can type commands from the Available Commands list, given below.

The command 'prompt' is the final line displayed:

> 2000 > 

User commands are entered at this prompt via the serial port.

The 2000 represents the CURRENT ADDRESS in HEX. Many commands default to their actions interacting with memory at this address. The CURRENT ADDRESS changes as you interact with TMON e.g. inputting code and data, and can be set by the ADDR command.

If you get lost, try entering ? to be reminded about the available commands.

The comand input editor is very simple. Invalid inputs are typically ignored, and results in the user simply being returned to the command prompt. The maximum command length accepted is 20 characters, however presently the longest valid command possible is 9 characters in length. When the user's input exceeds the maximum command length, the SC/TEC will emit a beep tone to indicate this condition has been reached. Backspace is supported, to correct typo's.

All data entered at all times is assumed to be HEX - 4 bytes for addresses, 2 bytes for data. Invalid data input is ignored.

### DATA mode

When the DATA command is given, TMON switches to interactive data entry mode. This is signified by the prompt changing as follows:

> XXXX NN :

XXXX continues to represent the CURRENT ADDRESS however the NN represents the HEX byte stored at that address, which you are presently editing.

- Enter a HEX byte and it will be written to memory at CURRENT ADDR; CURRENT ADDR is then incremented by one. 
- Pressing ENTER without inputting a new value leaves the existing value as-is and increments CADDR. In this way any bytes you don't wish to modify are skipped over.
- Enter the key word DIS (instead of a hex byte) to disasemble the current instruction at CADDR. This allows you to check the data you're entering is at the code point you expect it to be. Note that if you enter DIS while you are on the second or third byte of a multi-byte opcode, the disasembled instruction will be incorrect -- DIS always assumes the current address points to the first byte of an instruction.
- Type Q to exit data entry mode.

The DATA entry system is very simple and will continue to be improved in future versions.


## TMON Available Commands

### Help and General commands

EXIT  - Quits TMON and returns you to SCMON

HELP  - Displays some Helpfull information about how to use TMON

? - Displays a List of TMON commands

LIST - List is an alias for the HELP command

CLS - clears the terminal screen

### File Transfer commands

INTEL - tranfer an Intel HEX file from PC to SC/TEC

### Programming Related commands

TMON's CURRENT ADDRESS is used by the commands in this section, however this can be over-ridden if a specifc HEX address is given as part of the command. Having the CURRENT ADDRESS automatically update as a result of the command as it runs can be a handy thing e.g. when DUMPing or DISassembling blocks of memory.

INC ON/OFF - set auto-increment mode of CURRENT ADDRESS. No parameter supplied = Display current auto-increment mode.

Note - the xxxx represents a memory address input in hexadecimal. xxxx is optional; if specified it will override but not alter the 'current address' pointer. If not specified, the command acts based on the 'current address'.

ADDR xxxx - Set the CURRENT ADDRESS. If no address supplied, display the CURRENT ADDRESS instead.
GO xxxx - Execute code. The is the equivalent of pressing GO on a classic TEC or Fn 0 on an SC.
DUMP xxxx - DUMP the contents of 64 bytes of memory; provides HEX and ASCII outputs so memory can be examined. 
DIS xxxx - Disasemble Z80 instructions. Provides a disassembly of the opcodes found in memory, 16 opcodes at a time

DUMP and DIS pause at completion - space repeats the command (address continues to count up if auto-increment is on; otherwise same block repeats). Q quits and returns to the command prompt.

DATA xxxx - Interactively Input data into memory. Input one hex byte at a time; the value input is saved to the CURRENT ADDDRESS. Enter Q to quit input mode.

### Test & Informational commands

BEEP  - makes a beep sound on the SC/TEC

BELL - makes a bell sound on the serial terminal

VER - Displays the TMON and SCMON versions

HARDWARE - Returns the machine type - SC-1 or TEC-1F, and the keyboard type, Hardware (74c923) or Software (scanned)

RAMCHK - Runs a test to determine the size and location of any RAM within the Z80's 64k address space. Will pick up multiple RAM blocks even if not contiguous.

### Other commands

SCSM - Calls SCMON's built in serial monitor

7SEG - Displays the CURRENT ADDRESS and byte of memory on the TEC/SC 7-seg displays until a TEC/SC key is pressed. 0 exits to TMON; + and - TEC/SC keys increments/decrements CURRENT ADDRESS and continues.

SMON - Serial data steam monitor. Accepts serial input from the terminal and displays the HEX bytes received on screen. Great for debugging terminal comms and understaning control codes received from the PC (e.g. VT100 sequences). Enter Q (capital) to exit SMON.

## Changelog

### 0.4b

- Prep for first public release
- Added command for testing 7 seg displays
- Improvements to DATA:
  - Allow skipping bytes that don't need changing by just pressin ENTER
  - Allow on the fly disassembly to check code
- Added BELL command

### 0.3b

- Convert all Systems Calls from hard coded to SCMON includes file.
- Added keyboard type.
- Added command for calling SCSM

### 0.2b

- Add Jim's Disassembler
- Add single-word commands

### 0.1b

- First working code
- One letter commands
