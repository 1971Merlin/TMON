## Changelog

### 1.1
- 6850 command added. Supports use of 6850 Serial port Chip for input and output, instead of SC-1 bitbang
- FILL command added. Fill a block of memory with a byte

### 1.0
- First Public Release
- Removed redundant LIST command

### 0.4b
- Prep for first public release
- Improvements to DATA:
  - Allow skipping bytes that don't need changing by just pressing ENTER
  - Allow on the fly disassembly to check opcodes as you are editing
- Added 7SEG, BELL, MON, HALT, KEYTEST and REGS commands

### 0.3b
- Convert all Systems Calls from hard coded to SCMON includes file
- Added keyboard type
- Added command for calling SCSM

### 0.2b
- Add Jim's Disassembler
- Add single-word commands

### 0.1b
- First working code
- One letter commands
