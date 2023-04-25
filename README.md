# TMON
TMON (short for Terminal MONitor) is a complete serial port based monitor for the SC-1 and TEC-1F series of Z-80 computers. TMON uses the 'bit banged' serial port that is built into the machine, in conjunction with the 'Southern Cross Monitor' a.k.a. SCMON monitor ROM by Craig Jones.

The program runs without alteration on either the Talking Electronics TEC-1F or the Southern Cross SC-1 hardware, as it uses the SCMON System Calls library to communicate with the hardware of either machine.

TMON therefore requires the SCMON Monitor, version 1.8 or higher, available from https://github.com/crsjones/Southern-Cross-Computer-z80/tree/main/SouthernCrossSBC_Monitor be sure to use the correct version for your hardware.

## Requirements

- A Southern Cross SC-1 computer, or, a TEC-1F computer (Will not work on TEC-1 to TEC-1D)
- The TEC-1F must be set up for 8k addresing; TMON requires a little over 4k of RAM
- SCMON, The Southern Cross monitor, version 1.8 or newer
- Bit-banged serial interface connected to a PC or serial terminal
- The serial port is configured with the standard SCMON settings - 4800bps,N,8,2 - based on a 4MHz Z-80 clock
- Terminal Emulation type VT100 is required (most serial terminal programs e.g. PuTTY support VT100 by default)

## How to use TMON

TMON is an interactive tool, that works with a serial terminal e.g. PuTTY or Tera Term on an PC, or a 'real' VT100 serial terminal such as a Wyse WY-60. The SC/TEC keypad and 7-seg displays are not used once the program starts, and do not do anything (except for the testing routines documented below).

Interactions with TMON are via the serial console. The user types commands interactively and the results are displayed on the terminal.

### Starting up TMON

Transfer TMON to the SC/TEC by your favourite means - I use SCMON's built in Intel HEX transfer; it takes about 25 seconds to transfer the ~4k of HEX code. TMON loads into memory from address 2000h. You can also write it to NVRAM; however it will not run from ROM as-is, since the internal variables expect to be stored in RAM.

Once you have TMON loaded onto the SC/TEC, run it in the usual fashion and look at your serial terminal.

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

````
2000 > 
````

User commands are entered at this prompt via the serial port.

The 2000 represents the CURRENT ADDRESS in HEX. Many commands default to their actions interacting with memory at this address. The CURRENT ADDRESS changes as you interact with TMON e.g. inputting code and data, and can be set by the ADDR command. By default, TMON points to itself.

If you get lost, try entering ? or HELP to be reminded about the available commands.

The comand input editor is very simple. Invalid inputs are typically ignored, and results in the user simply being returned to the command prompt. The maximum command length accepted is 20 characters, however presently the longest valid command possible is 9 characters in length. When the user's input exceeds the maximum command length, the SC/TEC will emit a beep tone to indicate this condition has been reached. Backspace is supported, to correct typo's.

All data entered at all times is assumed to be HEX - 4 bytes for addresses, 2 bytes for data. Invalid data input is ignored.

### DATA mode

When the DATA command is given, TMON switches to interactive data entry mode. This is signified by the prompt changing as follows:

````
XXXX NN :
````

XXXX continues to represent the CADDR however the NN represents the HEX byte stored at that address, which you are presently editing.

- Enter a **HEX byte** and it will be written to memory at CADDR; CURRENT ADDR is then incremented by one. 
- Pressing **ENTER** increments CADDR by one and leaves the existing value as-is. In this way any bytes you don't wish to modify are skipped over.
- Enter **-** and CADDR will be decremented by one. This allows for correcting input errors and checking your input by following up with DIS command.
- Enter the key word **DIS** (instead of a hex byte) to disasemble the current instruction at CADDR. This allows you to check the data you're entering is at the code point you expect it to be. *Note that if you enter DIS while you are on the second or third byte of a multi-byte opcode, the disassembled instruction will be incorrect as DIS assumes that CADDR points to the first byte of an instruction.*
- Enter **Q** to exit data entry mode.

The DATA entry system is very simple and will continue to be improved in future versions.


## TMON Available Commands

### Help and General commands

**EXIT**  - Quits TMON and returns you to SCMON

**HELP** - Displays some Helpfull information about how to use TMON

**?** - Displays a List of TMON commands

**CLS** - Clears the terminal screen

### File Transfer commands

**INTEL** - Transfer an Intel HEX file from PC to SC/TEC

### Programming Related commands

TMON's CURRENT ADDRESS or CADDR is used by the commands in this section, however this can be over-ridden if a specifc HEX address is given as part of the command. Having CADDR automatically update as a result of the command as it runs can be a handy thing e.g. when DUMPing or DISassembling blocks of memory.

**INC ON/OFF** - set auto-increment mode of CADDR. No parameter supplied = Display the current auto-increment mode. Sometimes turning CADDR off is helpful.

*Note for the following commands - the xxxx represents an optional memory address input in hexadecimal. If specified the address will override but not alter the CADDR pointer. If not specified, the command acts based on the 'current address'.*

**ADDR xxxx** - Set the CURRENT ADDRESS. If no address supplied, display the CADDR instead.

**GO xxxx** - Execute code. The is the equivalent of pressing GO on a classic TEC or Fn 0 on an SC.

**DUMP xxxx** - DUMP the contents of 64 bytes of memory; provides HEX and ASCII outputs so memory can be examined. 

**DIS xxxx** - Disassemble Z80 instructions. Provides a disassembly of the opcodes found in memory, 16 opcodes at a time. The disassembler is an improved version of Jim Robertson's disassembler from Talking Electronics days; the two original design omisions have been added in, the code can now run from anywhere in memory, and various optimizations have been applied. Many thanks to Brian Chiha for his work on the disassembler.

DUMP and DIS pause at completion - space repeats the command (CADDR continues to increment if auto-increment is on; otherwise same block repeats). Q quits and returns to the command prompt. This allows you to quickly run through larger blocks without needing to type commands repeatedly.

**DATA xxxx** - Interactively Input data into memory. Input one hex byte at a time; the value input is stored to the CADDR memory location. Enter Q to quit input mode. Enter DIS to disassemble the instruction at the present location. See full description of DATA mode, above.

**FILL xxxx yyyy nn** - Fill memory between address xxxx and yyyy with data nn. note: Fill range must be at least 2 bytes long. Does not do any checks for safely - use with caution, as you can overwite any area of memory including TMON itself.

### Test & Informational commands

**BEEP** - Makes a beep sound on the SC/TEC

**BELL** - Makes a bell sound on the serial terminal

**VER** - Displays the TMON and SCMON versions

**HARDWARE** - Returns the machine type - SC-1 or TEC-1F, and the keyboard type, Hardware (74c923) or Software (scanned)

**RAMCHK** - Runs a test to determine the size and location of any RAM within the Z80's 64k address space. Will pick up multiple RAM blocks even if not contiguous.

**HALT** - Executes a CPU HALT instruction - on TEC-1F, press any key to resume. On SC-1, external hardware is required to generate an INT or NMI (e.g. single stepper) to resume.

**REGS** - Returns a dump of the Z80's registers. Note the values returned are typically always the same since TMON's internal state is pretty much always identical. Is useful for seeing if, for example, TMON or code you run with a GO is trashing the stack.

**KEYTEST** - Tests the TEC/SC's keyboard - the pressed key's scancode will appear on the 7-segment displays for as long as the key is held down. "Sh" (Shift -- TEC-1F only) is also displayed, if the shift key is also pressed a tthe same time. Note: the Shift state is only displayed in conjunction with a regular key also being pressed.

*Note the returned scancode is subject to adjustment by SCMON for compatability across hardware; it is not the 'raw' value returned by the 74c923.*
*Also note that presently there is no way to exit KEYTEST other than to reset the computer. This will be addressed in a future version.*

### Other commands

**SCSM** - Calls SCMON's built in serial monitor. Exiting SCMON returns you to TMON.

**7SEG** - Displays the CADDR and byte of memory on the TEC/SC 7-seg displays until a TEC/SC key is pressed. 0 exits to TMON; + and - TEC/SC keys increments/decrements CADDR and continues.

**SMON** - Serial data stream monitor. Accepts serial input from the terminal and displays the HEX bytes received on screen. Great for debugging terminal comms and understaning control codes received from the PC (e.g. VT100 sequences). Enter Q (capital) to exit SMON. This is a crude implementation, but does display the limitations of the bitbang serial in not being able to adequately buffer incoming bytes in real time (try pressing an arrow key or a PC function key).

If a terminal program such as Tera Term is used to add a small delay (e.g 20ms) between bytes transmitted from the PC, SMON can accurately show VT100 control codes such as a PC arrow or function key. Without the delay, the bitbang serial normally gets the first byte only, or perhaps the first and fourth or fifth byte, hence demonstrating the limitations of the bitbang interface.

### Assembling TMON from source

TMON assembles using Telemark TASM asembler. The syntax is fairly generic Z80 and should assemble with minimal changes on most assemblers.

Note that you will also require the SCMON INCLUDE file scm18_include.asm from SCMON - available from the SCMON link at the top of this document.

TMON may be compiled to run from anywhere in memory space by altering the .ORG statement at the top of the code. There are no hard coded addresses. The SCMON includes file also takes care of all the hardware-specific differences beteween the TEC-1F and the SC-1..oyu do not need to reassemble the code to switch from one machine to the other. Just ensure the machine is running the version of SCMON designed for that hardware.

As TMON is open source and licenced under GPLv3, feel free to fix bugs, add features and whatnot using the normal GitHub tools and in accordance with GPL principals.
