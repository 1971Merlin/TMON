; -----------------------------------------------------------------------------
; TMON - a serial monitor for the SC-1 and TEC-1F computers
;
; Requires SCMON; the Southern Cross Monitor, version 1.8 or newer
;
; Written by Craig Hart and released under the GPL v3 license
;
; https://github.com/1971Merlin/TMON
;
;
; Disassembler code by Jim Robertson with improvements by Brian Chiha
; and further updates by Craig Hart
;
; -----------------------------------------------------------------------------

; bring in SCMON library

#include "C:/Users/user/OneDrive/TEC-1/SC-1/TMON/scm18_include.asm"

	.org 2000h

	ld a,0
	ld (smode),a		; set to bitbang
	call Sinit		; setup 6850, if present


init:	ld a,0ffh		; assume 1.7 or older; ffh = old!
	ld c,VERS		; get mon version
	rst 30h

	cp 0ffh			; 1.8+ modifies A; older ones don't
	jp nz, newmn

	halt			; sorry, too old!!

newmn:	ld c,SERINI		; serial init - 1.8+ only
	ld hl,0			; 0 = default
	rst 30h

	ld c,BEEP		; beep
	rst 30h

	ld hl,clrhome		; clear terminal screen
	call smsg

; welcome screen

	call vers		; display ROM version
	call hardware		; display machine type
	call ramfind		; display RAM stats

	ld hl,2000h
	ld (caddr),hl		; setup system location


mainloop:
	call uprompt		; draw user prompt
	call usin		; set a line from the user
	call usparse		; parse that line
	call match		; look for a valid command & do it

	jr mainloop

; -----------------------------------------------------------------------------
; uprompt - draw the default user input prompt
; -----------------------------------------------------------------------------

uprompt:
	ld de,(caddr)		; fill up prompt with current address
	ld hl,prompt		; where to write it
	ld c,WRDASC
	rst 30h

	ld hl,prompt		; display main prompt
	call smsg

	ret

; -----------------------------------------------------------------------------
; parameter parser - extracts hex address(es) and/or data from command params
;
; input: uparam and uparaml populated with 4 hex digits
;
; not a lot of error checking so far
;
; returns:	HL = first hex word
;		BC = second hex word (if present)
;		A = third hex byte (if present)
;		CF = 1 = no data available
; -----------------------------------------------------------------------------

pp:	ld a,(uparaml)
	cp 4
	ret c			; exit with Z flag set if <4 chars
	jr z,do4b

	cp 9
	jr z,do8b

	cp 12
	jr z,do12b

	scf			; in order to return C set with ret nz
	ret nz			; must be exactly 4 or 9 bytes


do12b:	ld hl,uparam+10
	call pp1
	ld (bytbuf),a

do8b:	ld hl,uparam+5		; second parameter
	call pp1
	ld b,a

	ld hl,uparam+7
	call pp1
	ld c,a


do4b:	ld hl,uparam		; first parameter
	call pp1
	ld d,a

	ld hl,uparam+2
	call pp1
	ld e,a

	ld h,d
	ld l,e
	ld a,(bytbuf)
	or a			; clears carry flag
	ret


pp1:	push bc
	ld a,(hl)		; 2nd digit
	inc hl

	call a2hex
	rrca			; move to upper nibble
	rrca
	rrca
	rrca
	ld c,a			; save it
	ld a,(hl)		; 1st digit
	call a2hex
	or c			; add in other bit
	pop bc
	ret

a2hex:	sub 30h
	cp 10
	ret c
	sub 7
	ret

; -----------------------------------------------------------------------------
; Command input matcher
;
; Finds and Executes any matched commands
; -----------------------------------------------------------------------------

match:
	ld hl,cmds

cloop:	ld de,ucmd
	ld a,(ucmdl)
	ld b,a

	ld a,(hl)		; outerloop; 0xff terminated
	cp 0ffh
	jp z, notfound

; a = length of command
; b = length of input

	ld c,a			; save it
	inc hl			; start of string

	cp b			; cant be if different lengths
	jp nz,notm

	push hl
	call cmp
	pop hl

	jp z, found		; z = matched

notm:
	inc c			; skip 2 bytes
	inc c
	ld b,0
	add hl,bc		; next element

	jp cloop
found:

	ld b,0			; c already contains length
	add hl,bc		; HL already points at string start
	ld d,h
	ld e,l

	ld a,(de)
	ld l,a
	inc de
	ld a,(de)
	ld h,a

	jp (hl)			; the RET of the command returns to main loop

notfound:
	ret			; no match

; -----------------------------------------------------------------------------
; string compare
;
; HL - source
; DE - compare
; B - # bytes
;
; Z flag set = matched
; -----------------------------------------------------------------------------

cmp:	ld a,(de)
	cp (hl)
	ret nz			;nz means they are not equal

	inc hl
	inc de
	djnz cmp

	cp a			; set Z flag for success
	ret

; -----------------------------------------------------------------------------
; Command input processor
;
; populates
;	ucmd with asciiz of user input delimited by first space
;	ucmdl with length not including the null
;
;	uparam with asciiz of user input beyond first space
;	uparaml with length not including the null
; -----------------------------------------------------------------------------

usparse:
	xor a
	ld (ucmdl),a
	ld c,a			; set counter

	ld a,(sbufl)
	or a
	jp z, pdone

	ld b,a			; max length
	ld hl,sbuf		; from
	ld de,ucmd		; to

uloop:	ld a,(hl)
	or a
	jr z,udone1
	cp 20h			; space char?
	jr z,drest

	ld (de),a
	inc hl
	inc de

	inc c

	djnz uloop		; next until all done

	jp udone1


; if here we got a space - so get the rest of the input
; HL = position in input buffer ucmd
; B = bytes left to go
; C = length of first string

; first finish cleanup of  first string


drest:	xor a			; null terminate it
	ld (de),a

	ld a,c
	ld (ucmdl),a		; store length not incl null


	ld a,b
	cp 1			; space on the end ???
	jp z, pdone

; setup new counters
	ld de,uparam		; new to buffer
	xor a
	ld (uparaml),a		; new length zero bytes
	ld c,a
	inc hl			; skip space

ploop:	ld a,(hl)
	or a
	jr z,pdone1

	ld (de),a
	inc hl
	inc de

	inc c

	djnz ploop		; next until all done

pdone1:	xor a			; null terminate it
	ld (de),a

	ld a,c
	ld (uparaml),a		; store length not incl null

	jr pdone

; ucmd now contans the first string only - one word typed in

udone1:	xor a			; null terminate it
	ld (de),a

	ld a,c
	ld (ucmdl),a		; store length not incl null

	xor a
	ld (uparam),a
	ld (uparaml),a		; zero out : no parameters

pdone:	ret

; -----------------------------------------------------------------------------
; Command input editor - user interactive
;
; populates	sbuf with user input string
;		sbufl with length of string input in bytes
; -----------------------------------------------------------------------------

usin:	xor a			; no chars input
	ld (sbufl),a
	ld (sbuf),a

sbuflp:

	call srx		; wait for a byte input


; future VT100 support
; bitbang can't keep up with serial data stream though


vt:	cp 01bh			; ESC -- special key
	jr nz,bsp
;	ld c,RXDATA
;	rst 30h			; now get byte 2
;	cp 01bh
;	ret z			; esc esc = outta here
;	cp 05bh			; "[" ?
;	jr nz,sbuflp
	call beep
	jr sbuflp


bsp:	cp 07fh			; special chars? 7f = backspace
	jr nz,cr

; backspace code
	ld a,(sbufl)		; cant bksp beyond start
	or a
	jr z,sbuflp
	dec a
	ld (sbufl),a

	ld hl,bkspc		; back one char
	call smsg

	ld a,32			; space -- erase old text
	call stx

	ld hl,bkspc		; and back again
	call smsg

	jr sbuflp


cr:	cp 0dh			; 0Dh = enter
	jr nz,stor

; pressed enter code
	ld hl,sbuf
	ld a,(sbufl)
	ld c,a
	ld b,0
	add hl,bc

	xor a			; add the 0 on the end of string
	ld (hl),a

	ld hl,crlf
	call smsg

	ret			; we done, exit


stor:	ld d,a			; store for later our character

	ld a,(sbufl)
	cp inplen-7		; ***MAX LINE LENGTH IN chars less prompt ***
	jr z,bfull		; but not if full

	ld hl,sbuf		; add to buffer
	ld c,a
	ld b,0
	add hl,bc

	ld a,d			; our character restored
	ld d,c			; store buff length

	call stx

	call toupper
	ld (hl),a		; put in buffer

	inc d			; +1 buffer length
	ld a,d
	ld (sbufl),a
	jr sbuflp

bfull:

	ld c,BEEP		; beep = error!!
	rst 30h

	jr sbuflp

; -----------------------------------------------------------------------------
; SMON - dump serial input as hex bytes
; -----------------------------------------------------------------------------

banglp:
	call srx

	cp 051h			; ? "Q"
	jr z,exsmon

	ld b,a
	call mkprnt

	ld hl,wordbuf
	ld (hl),a		; store our character
	inc hl

	ld a,b
	ld c,BYTASC
	rst 30h

	ld a,32			; space
	ld (hl),a

	dec hl
	dec hl
	dec hl

	call smsg

	jr banglp

exsmon:	ld hl,crlf
	call smsg

	ret

; -----------------------------------------------------------------------------
; TOUPPER convert text to uppercase; not a perect conversion
; -----------------------------------------------------------------------------

toupper:
	cp 61h
	jr c, less
	sub 20h

less:	ret

; -----------------------------------------------------------------------------
; ramfind - locate block(s) of RAM and display info about them
; -----------------------------------------------------------------------------

ramfind:
	ld hl,0h			; setup start address
	ld (ramtst),hl

	xor a				; 1 = we are already a block
	ld (ramflag),a


rloop:	ld hl,(ramtst)
	call rtst			; does it work ?
	jr z,ok				; Z flag set = ram present, go on

	ld a,(ramflag)			; end of block found ?
	cp 1
	jr z, dumpp
	jr cont

dumpp:	call dumpblk			; end of the block was reached

	xor a
	ld (ramflag),a

	ld hl,(ramtst)			; carry on looking for next block
	jr cont

ok:	ld a,(ramflag)			; in a block ?
	cp 1
	jR z,ok2

	ld (ramst),hl			; new block!!
	ld a,1				; set flag
	ld (ramflag),a

ok2:	ld (ramend),hl			; update location of end of ram

cont:	ld bc,adinc			; next block
	add hl,bc
	ld (ramtst),hl

	ld a,h
	or a
	jr z,rtdone			; if H=0 we are done (wrap round)

	jr rloop			; otherwise test next block

rtdone:
	ld a,(ramflag)
	or a
	jr z, doneit

	call dumpblk			; all ram ?

doneit:	ret

; -----------------------------------------------------------------------------
; HELP command
; -----------------------------------------------------------------------------

help:	ld hl,helpstr			; display message
	call smsg
	ld hl,helpst2			; display message part 2
	call smsg
	ret

; -----------------------------------------------------------------------------
; EXMON command; returns to monitor
; -----------------------------------------------------------------------------

exmon:	ld c,MAIN
	rst 30h
	ret

; no RET in reality - we never coming back!!

; -----------------------------------------------------------------------------
; INTEL command; RX Intel HEX file
; -----------------------------------------------------------------------------

intel:
	ld hl,intelhx			; message
	call smsg

	ld c,INTELH
	rst 30h

	ld hl,success			; good load
	jp z, imsg
	ld hl,failure			; bad load

imsg:	call smsg
	ret

; -----------------------------------------------------------------------------
; LIST command; show all commands
; -----------------------------------------------------------------------------

lst:	ld hl,cmds		; start off
	xor a
	ld (llen),a


listl:	ld a,(hl)
	cp 0ffh			; done?
	jp z, outs		; if done then return

	inc hl

	ld c,a
	ld a,(llen)
	add a,c
	inc a			; + space
	ld (llen),a

	ld b,0
	ld de,ucmd
	ldir			; copy commmand to buffer

	ld a,20h		; add space
	ld (de),a
	inc de
	xor a			; add null terminator
	ld (de),a

	push hl

	ld hl,ucmd		; show command
	call smsg

	ld a,(llen)
	cp 75
	jr c, contn

	call outs

	xor a
	ld (llen),a

contn:	pop hl

	inc hl			; skip the jp bytes
	inc hl

	jp listl


outs:	ld hl,crlf
	call smsg

	ret

; -----------------------------------------------------------------------------
; BEEP command; make a beep sound on the TEC/SC
; -----------------------------------------------------------------------------

beep:	ld c,BEEP
	rst 30h
	ret

; -----------------------------------------------------------------------------
; BELL command; make a beep sound on the serial console
; -----------------------------------------------------------------------------

bell:	ld a,07h		; 7 = bell
	call stx
	ret

; -----------------------------------------------------------------------------
; VER command; return version strings
; -----------------------------------------------------------------------------

vers:
	ld hl,tmonver		; display boot message
	call smsg

	ld hl,romver		; display ROM Version message
	call smsg

	ld c,VERS		; get monitor version
	rst 30h
	call smsg		; display it

	ld hl,crlf
	call smsg

	ret

; -----------------------------------------------------------------------------
; HARDWARE command; return target hardware platform; TEC or SC
; -----------------------------------------------------------------------------

hardware:
	ld hl,plat		; display platform message

	call smsg

	ld c,PCBTYP		; get target platform => HL points to string
	rst 30h
	call smsg

	call prtinl
	.db ", ",0

	ld c,KBDTYP		; get keyboard type => HL points to string
	rst 30h
	call smsg

	call prtinl
	.db " keyboard",13,10,0

	ret

; -----------------------------------------------------------------------------
; CLS command; clear screen
; -----------------------------------------------------------------------------

cls:	ld hl,clrhome
	call smsg
	ret

; -----------------------------------------------------------------------------
; SCBUG command; call SCMON built-in Serial Monitor
; -----------------------------------------------------------------------------

scmon:	ld c,SCBUG
	rst 30h
	ret

; ---------------------------------------------------------------------------
; ACIA command; control serial output mode; bitbang or 6850
; -----------------------------------------------------------------------------

acia:	ld a,(uparaml)
	or a
	jr z,shacia		; skip if no input


	ld hl,uparam
	ld de,onstr
	ld b,3
	call cmp
	jr nz,ntone

	ld a,1
	ld (smode),a
	jr shacia


ntone:
	ld hl,uparam
	ld de,offstr
	ld b,4
	call cmp
	jr nz,shacia

	xor a
	ld (smode),a

shacia:
	ld hl,aciastr		; message part 1
	call smsg
	ld hl,incon

	ld a,(smode)
	cp 1
	jr z, outnc

	ld hl,incoff

outnc:
	call smsg

	ret

; ---------------------------------------------------------------------------
; INC command; control automatc updating of CADDR
; -----------------------------------------------------------------------------

autoinc:
	ld a,(uparaml)
	or a
	jr z,shinc		; skip if no input


	ld hl,uparam
	ld de,onstr
	ld b,3
	call cmp
	jr nz,notone


	ld a,1
	ld (autinc),a
	jr shinc


notone:
	ld hl,uparam
	ld de,offstr
	ld b,4
	call cmp
	jr nz,shinc

	xor a
	ld (autinc),a

shinc:
	ld hl,incstr		; message part 1
	call smsg

	ld hl,incon

	ld a,(autinc)
	cp 1
	jr z, outinc

	ld hl,incoff

outinc:
	call smsg

	ret

; -----------------------------------------------------------------------------
; GO command; run somenthing; no parameter = go from caddr
; -----------------------------------------------------------------------------

go:	call pp
	jr nc,exec
	ld hl,(caddr)

exec:	jp (hl)
	ret

; -----------------------------------------------------------------------------
; SETADDR command;update current edit address
; -----------------------------------------------------------------------------

setaddr:
	call pp
	jr c, shaddr		; exit if C set = bad/no input
	ld (caddr),hl
	ret

shaddr:
	ld de,(caddr)		; fill up prompt with current address
	ld hl,wordbuf		; where to write it
	ld c,WRDASC
	rst 30h

	ld hl,wordbuf		; display addr
	call smsg

	ld hl,crlf		; newline
	call smsg

	ret

; -----------------------------------------------------------------------------
; DISASSEMBLE command; disassemble the Z-80 instruction at current address
; -----------------------------------------------------------------------------

disassemble:
	call pp
	jr c,dinp
	ld (caddr),hl

dinp:	ld hl,(caddr)
	ld (PERFROM),hl		; PERFROM is maintained by Jim's disasembler
				; always points to start of next instruction to
				; be disassembled

	ld b,16			; loop 16 instructions

disblk:	push bc

	call DIS_START		; do it
	call disout		; and display the result

	pop bc

	djnz disblk

	ld hl,(PERFROM)		; do we update CADDR?

	call pauser
	jr z,dinp

	ret

; -----------------------------------------------------------------------------
; DISOUT - displays the disassembler's output string
; -----------------------------------------------------------------------------


disout:	ld hl,(DISEND)		; null terminate our string
	ld (hl),0

	ld hl,DISFROM		; show output buffer line 1
	call smsg

	ld hl,crlf		; and tidy up
	call smsg

	ret

; -----------------------------------------------------------------------------
; incchk - updates CURRENT ADDRESS if mode turned on
; HL = new address to be used if inc mode is selected
; -----------------------------------------------------------------------------

incchk:	ld a,(autinc)		; update caddr?
	cp 1
	ret nz

	ld (caddr),hl
	ret

; -----------------------------------------------------------------------------
; Pause-er. Space to continue, Q to quit
; -----------------------------------------------------------------------------

pauser:
	call incchk			; update address?
	call srx
	call toupper

	cp 20h				; space
	ret z				; C flag set = continue

	cp 51h
	jr nz,pauser

	cp 20h				; clear zero flag; fail on purpose

	ret

; -----------------------------------------------------------------------------
; DATAINP command;input data interactively from the user
; -----------------------------------------------------------------------------

datainp:
	call pp
	jr c,cinp
	ld (caddr),hl


cinp:	ld de,(caddr)		; fill up prompt with current address
	ld hl,inprmpt		; where to write it
	ld c,WRDASC
	rst 30h

	inc hl

	ld de,(caddr)
	ld a,(de)
	ld c,BYTASC		; and put into string
	rst 30h

	ld hl,inprmpt		; display main prompt for data entry mode
	call smsg

	call usin		; get the user's input

	ld a,(sbufl)

	or a
	jr z, ent		; 0 chars == pressed enter

	cp 1			; 1 char ?
	jr z, qut

	cp 2			; 2 chars exactly ?
	jr z, inpb

	cp 3			; 3 chars, could be dis command?
	jr z, isdis

	jr cinp			; invalid entry : do nothing


qut:	ld a,(sbuf)
	cp 51h 			; "Q" = QUIT entry mode
	ret z			; bail out of data entry mode

	cp 2dh			; "-" = decrement CADDR
	jr nz,cinp

	ld hl,(caddr)
	dec hl
	ld (caddr),hl

	jr cinp			; if not a valid char: do nothing

; assuming enter here; sbuf and sbufl = 00

ent:	ld hl,(caddr)		; pressed ENTER so inc. caddr
	inc hl
	ld (caddr),hl

	jr cinp

isdis:	ld hl,disstr		; is it "DIS" ?
	ld de,sbuf
	ld b,4
	call cmp

	jr nz,cinp		; nope


	ld hl,(caddr)		; yes, so disassemble that instruction
	ld (PERFROM),hl
	ld b,1

	call DIS_START
	call disout

	jr cinp

; if here we have a byte to enter

inpb:	ld a,(sbuf+1)
	ld c,ASCHEX
	rst 30h

	ld b,a

	ld a,(sbuf)
	rst 30h
	rrca
	rrca
	rrca
	rrca
	add a,b

	ld hl,(caddr)
	ld (hl),a
	inc hl
	ld (caddr),hl

	jp cinp

; -----------------------------------------------------------------------------
; SEGTEST command - exercises the 7-seg displays and keyboard
; -----------------------------------------------------------------------------

segtest:
	ld c,CLRBUF
	rst 30h

	ld hl,(caddr)
	ld c,DISADD
	rst 30h

	ld a,(hl)
	ld c,DISBYT
	rst 30h

	ld c,SKEYIN
	rst 30h

	or a				; pressed 0 ?
	ret z

	cp KEYINC
	jr nz, tstdec

	ld hl,(caddr)
	inc hl
	ld (caddr),hl
	jr segd

tstdec:	cp KEYDEC
	jr nz,segd

	ld hl,(caddr)
	dec hl
	ld (caddr),hl

segd:	ld c,SKEYRL
	rst 30h

	jr segtest


; -----------------------------------------------------------------------------
; DOHALT - Halt the CPU
; -----------------------------------------------------------------------------

dohalt:	halt
	ret

; -----------------------------------------------------------------------------
; KEYBTEST - test the keyboard -> 7seg
; -----------------------------------------------------------------------------

keybtst:
	ld c,CLRBUF
	rst 30h

	ld c,SCANKEY
	rst 30h

	bit 5,a
	jr z,keybtst

	ld b,a				; backup A
	and 01fh			; mask high bits

	ld c,DISBYT
	rst 30h

	bit 6,b				; shift ?
	jr z,cn

	ld a,066h			; TEC 7-seg codes
	ld (DISBUF+4),a
	ld a,0a7h
	ld (DISBUF+5),a

cn:
	ld c,SCAND			; show something
	rst 30h

	jr keybtst


; -----------------------------------------------------------------------------
; DUMP command; dump a memory block; no parameter = dump from caddr
; -----------------------------------------------------------------------------

dump:	call pp
	jr c,usecad

	ld a,l
	and 0f0h		; align to 16 byte boundary
	ld l,a
	jr dval


usecad:	ld hl,(caddr)
	ld e,l
	ld a,l
	and 0f0h		; align to 16 byte boundary
	ld l,a

dval:	ex de,hl		; really, load DE from HL

	ld b,16			; 16 rows, outer loop

odlp:	push bc

	ld b,16			; 16 bytes, inner loop (one row)
	ld hl,dispbuf

	ld a,d			; address
	ld c,BYTASC
	rst 30h
	ld a,e
	ld c,BYTASC
	rst 30h
	ld a,20h
	ld (hl),a
	inc hl
	ld a,20h
	ld (hl),a
	inc hl


dlp:	ld a,(de)		; data
	inc de

	ld c,BYTASC
	rst 30h

	ld a,20h
	ld (hl),a
	inc hl

	djnz dlp

	ld a,20h
	ld (hl),a
	inc hl
	ld a,20h
	ld (hl),a
	inc hl

	ld b,16			; back to start of block
decde:	dec de
	djnz decde

	ld b,16

asl:	ld a,(de)		; ascii dump
	inc de

	call mkprnt		; clean up nonprintables

	ld (hl),a
	inc hl

	djnz asl

	xor a			; null terminate it
	ld (hl),a

	ld hl,dispbuf		; output the buffer (one line)
	call smsg

	ld hl,crlf
	call smsg

	pop bc

	djnz odlp

	ex de,hl		; load hl with the pointer

	call pauser
	jr Z,usecad

	ret


; -----------------------------------------------------------------------------
; BLOCKFILL command; fill a memory block with a byte
; -----------------------------------------------------------------------------

blockfill:

	call pp
	ret c			; bail if error

; hl = first
; bc = second
; a = value

	ld (blkval),a
	ld (blkst),hl
	ld (blkend),bc
	ld b,h
	ld c,l
	ld hl,(blkend)
	or a
	sbc hl,bc
	ld (blkbyt),hl
	ret z			; bail if 0 bytes to write


	ld de,(blkst)
	ld hl,fillmsg+14h
	ld c,WRDASC
	rst 30h

	ld de,(blkend)
	ld hl,fillmsg+1dh
	ld c,WRDASC
	rst 30h

	ld a,(blkval)
	ld hl,fillmsg+2eh
	ld c,BYTASC
	rst 30h

	ld hl,fillmsg
	call smsg

	ld hl,(blkst)		; setup ldir
	ld d,h
	ld e,l
	inc de
	ld a,(blkval)
	ld (hl),a
	ld bc,(blkbyt)

	ldir

	call prtinl
	.db "Wrote ",0

	ld hl,(blkbyt)
	inc hl
	call decimal

	call prtinl
	.db " bytes",13,10,0

	ret

; -----------------------------------------------------------------------------
; MKPRNT - make an ASCII value printable
;
; IN: A = byte to fix up
; OUT: A = fixed up byte
;
; -----------------------------------------------------------------------------

mkprnt:	cp 7fh			; > 7fh = unprintable
	jr nc,fix
	cp 32			; < 32 = unprintable
	jr nc,yok

fix:	ld a,2eh		; swap in a '.'

yok:	ret

; -----------------------------------------------------------------------------
; dumpblk - display info abbout the RAM block found
; -----------------------------------------------------------------------------

dumpblk:
	ld hl,rammsg

	call smsg

	ld de,(ramst)			; dump values to screen
	ld hl,wordbuf
	ld c,WRDASC
	rst 30h
	ld hl,wordbuf
	call smsg

	call prtinl
	.db "h and ",0

	ld de,(ramend)
	ld hl,wordbuf
	dec e				; adjust to xxffh
	ld c,WRDASC
	rst 30h

	ld hl,wordbuf
	call smsg


	call prtinl
	.db "h - ",0


; work out how many bytes
	ld hl,(ramend)
	ld bc,adinc
	add hl,bc

	ld de,(ramst)
	or a
	sbc hl,de

	call decimal			; HL -> output

	call prtinl
	.db " bytes",13,10,0

	ret

; -----------------------------------------------------------------------------
; DECIMAL - HL to decimal, sent out the serial one char at a time
; -----------------------------------------------------------------------------

decimal:
	ld e,1				; 1 = don't print a digit

	ld	bc,-10000
	call	Num1
	ld	bc,-1000
	call	Num1
	ld	bc,-100
	call	Num1
	ld	c,-10
	call	Num1
	ld	c,-1

Num1:	ld	a,'0'-1

Num2:	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc

	ld d,a				; backup a
	ld a,e
	or a
	ld a,d				; restore it in case
	jr z,prout			; if E flag 0, all ok, print any value

	cp '0'				; no test if <>0
	ret z				; if a 0, do nothing (leading zero)

	ld e,0				; clear flag & print it

prout:	push bc
	call stx
	pop bc

	ret

; -----------------------------------------------------------------------------
; REGS - Dump out the registers (for debugging)
; -----------------------------------------------------------------------------

regs:	push af
	push bc
	push de
	push hl
	push ix
	push iy

	push af			; junk; moves sP down for following push
	ld hl,0			; hl now points to current stack bottom
	add hl,sp
	pop af			; clear sp

	ld d,h
	ld e,l

	ld bc,0010h		; currstack + ret addr = original (push + 2 bytes)
	add hl,bc

	push hl			; save original SP


; de = stack bottom
; hl = original sp before CALL

	call prtinl
	.db "SP   AF   BC   DE   HL   IX   IY   PC",13,10,0


	ld b,8			; 8 register pairs to display


regloop:
	push bc
	push de

	ld h,d
	ld l,e

	ld e,(hl)		; DE
	inc hl
	ld d,(hl)

	ld (PERFROM),de		; save for disassembler, ends up being PC


	ld hl,wordbuf
	ld c,WRDASC
	rst 30h

	ld hl,wordbuf
	call smsg

	call prtinl
	.db	" ",0

	pop de
	pop bc

	inc de
	inc de

	djnz regloop


	ld hl,crlf
	call smsg

; ---

	ld b,1			; one instruction
	call DIS_START
	call disout

; ---

	pop hl			; junk; is really SP
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af

	ret



; -----------------------------------------------------------------------------
; RTST   checks a byte of memory to see if it can store and return values
;
; In:
; HL = memory location to check
;
; out:
; Z flag set if ram present
; DE trashed
; -----------------------------------------------------------------------------

; test code cant be at xx00h or it'll be overwriten by the tester
	.ORG (($ + 0FFH) & 0FF00H) +1	; align to next 256 byte boundary +1 byte

rtst:
	di			; disable interrupts; avoid memory corruption

	ld e,(hl)		; backup original value

	ld a,0aah		; test 1 store aa
	ld (hl),a
	nop
	ld d,(hl)
	cp d
	jr nz, noram

	ld a,055h		; test 2 store 55
	ld (hl),a
	nop
	ld d,(hl)
	cp d
	jr nz, noram

	ld a,00h		; test 3 store 00
	ld (hl),a
	nop
	ld d,(hl)
	cp d
	jr nz, noram

	ld a,0ffh		; test 4 store ff
	ld (hl),a
	nop
	ld d,(hl)
	cp d
	jr nz, noram		; Z flag set (=1), == ram present


noram:	ld (hl),e		; restore value (even if failed teset!)
				; if failed Z flag is reset (=0)
	ei			; end test enable interrupts
	ret

; -----------------------------------------------------------------------------
; Constants, Strings and Variables
; -----------------------------------------------------------------------------


; constants

adinc	.equ 0100h		; ram check block jump size

inplen	.equ 27			; max user input line length (add 7 for prompt)


; Strings

clrhome	.db	27,"[H",27,"[2J",0

bkspc	.db	27,"[1D",0

tmonver	.db	"TMON Version 1.1",13,10,0
romver	.db	"SCMON version: ",0
plat	.db	"Compiled Platform: ",0

rammsg	.db	"RAM Found between ",0

crlf	.db	13,10,0

helpstr	.db	"This is the HELP page",13,10
	.db	13,10
	.db	"Type a command and press Enter",13,10
	.db	"Type EXIT to quit to monitor",13,10
	.db	13,10,0
helpst2	.db	"Type HELP to display this information",13,10
	.db	"Type LIST or ? to see a list of commands",13,10,0

success	.db	"Success",13,10,0
failure	.db	"Failed",13,10,0

incstr	.db	"Auto-Increment mode is ",0
aciastr	.db	"6850 ACIA serial mode is ",0
incon	.db	"on",13,10,0
incoff	.db	"off",13,10,0

intelhx	.db 	"Ready to receive Intel HEX File...",0

onstr	.db	"ON",0
offstr	.db	"OFF",0

disstr	.db	"DIS",0


; format of cmds table : length in bytes of command, command, jmp address.
; terminates with 0xffh


cmds	.db	4,"HELP"
	.dw	help
	.db	1,"?"
	.dw	lst
	.db	4,"EXIT"
	.dw	exmon
	.db	5,"INTEL"
	.dw	intel
	.db	4,"BEEP"
	.dw	beep
	.db	4,"BELL"
	.dw	bell
	.db	3,"VER"
	.dw	vers
	.db	3,"CLS"
	.dw	cls
	.db	8,"HARDWARE"
	.dw	hardware
	.db	6,"RAMCHK"
	.dw	ramfind
	.db	2,"GO"
	.dw	go
	.db 	4,"DUMP"
	.dw	dump
	.db	4,"ADDR"
	.dw 	setaddr
	.db	4,"DATA"
	.dw	datainp
	.db	3,"DIS"
	.dw	disassemble
	.db	3,"INC"
	.dw	autoinc
	.db	4,"SCSM"
	.dw	scmon
	.db	4,"7SEG"
	.dw 	segtest
	.db	4,"SMON"
	.dw	banglp
	.db	4,"HALT"
	.dw	dohalt
	.db	7,"KEYTEST"
	.dw	keybtst
	.db	4,"REGS"
	.dw	regs
	.db	4,"6850"
	.dw	acia
	.db	4,"FILL"
	.dw	blockfill
	.db	0ffh			; FFh  = end of list


; Variables

ramst	.dw	0000h
ramtst	.dw	0000h
ramend	.dw	0000h

wordbuf	.db	"0000",0
bytbuf	.db	0

ramflag	.db 	1

autinc	.db	1

llen	.db	0

sbufl	.db	0
sbuf	.block	inplen

ucmdl	.db	0
ucmd	.block	inplen

uparaml	.db	0
uparam	.block	inplen


dispbuf	.block	80			; one line of terminal always

caddr	.dw	2000h			; current address

prompt	.db	"0000 > ",0
inprmpt	.db	"0000 00 : ",0

blkst	.dw 0
blkend	.dw 0
blkbyt	.dw 0
blkval	.db 0

fillmsg	.db "Filling Memory from 0000h to 0000h with value 00h",13,10,0



; JIMS DISASSEMBLER SOURCE CODE
; -----------------------------

; Written originally by Jim Robertson
;
; Revised version that can run from any memory location, developed by Brian Chiha
;
; updates by Craig Hart to fix remaining memory constraints and correct two deficiencies
; - IN (xx),A/OUT A,(xx) were missing their brackets
; - Conditional JPs (e.g. JP NC, zzzz) were missing the comma
; Also removed left over inactive code, fixed JP > JP > routine double JPs
;


; Local Variables
DISFROM:	.block 18
DISMID:		.block 18
DISEND:		.dw 0			; pointer to last valid char in disfrom
DISFLAG:	.db 0 			; Dis Flag for HL,IX,IY
PERFROM:	.dw 0 			; Start Address to disassemble from

DIS_START:
	LD	HL,DISFROM
	XOR	A
	LD	(DISFLAG),A
	LD	(DISEND),HL
	LD	HL,(PERFROM)
	PUSH	HL
	CALL	hltoascii

	LD	B,20h			; fill 32 spaces
	CALL	addspc

	ld hl,DISFROM			; move backwards in buffer
	ld bc,5
	add hl,bc

	LD	(DISEND),HL
	POP	HL
	LD	A,(HL)			; a = Opcode to decode
	PUSH	HL
	LD	D,01
	CALL	onebyt
	POP	HL
	JR	NC,foundone
	CALL	mltbyte
	LD	C,(HL)
	LD	A,(HL)
	CALL	NC,miscop

foundone:
	LD	HL,(PERFROM)
	INC	HL
	LD	(PERFROM),HL
	RET				; return to caller, we are done

mltbyte:
	CP	40h
	JR	C,L0x304D
	CP	0C0h
	JR	C,L0x304F
	CP	0CBh
	JP	Z,L0x31DE
	LD	C,00
	CP	0DDh
	JR	Z,L0x3049
	CP	0FDh
	JR	NZ,L0x304D
	INC	C
L0x3049:
	INC	C
	JP	L0x30D7
L0x304D:
	OR	A
	RET

L0x304F:
	PUSH	AF
	CALL	L0x3055
	JR	L0x305A
L0x3055:
	LD	B,01
	JP	L0x31FF
L0x305A:
	POP	AF
L0x305B:
	CP	80h
	JR	NC,L0x3073
	PUSH	AF
	CALL	L0x3205
	POP	AF
	CALL	L0x31AC
	PUSH	AF
	CALL	L0x3106
	CALL	addcomma
	POP	AF
	LD	C,A
	JP	L0x3107
L0x3073:
	AND	3Fh
	CALL	L0x31AC
	PUSH	AF
	LD	A,86h
	ADD	A,C
	ADD	A,C
	ADD	A,C
	CALL	L0x3153
	CALL	L0x3086
	JR	L0x3092
L0x3086:
	CALL	L0x317B
	RET	NZ
	LD	C,07
	CALL	L0x310B
	JP	addcomma
L0x3092:
	POP	AF
	LD	C,A
	JR	L0x3106
L0x3096:
	CP	40h
	JR	NC,L0x30B0
	CALL	L0x31AC
	PUSH	AF
	LD	A,C
	CP	07
	JR	NZ,L0x30A4
	DEC	C
L0x30A4:
	LD	A,9Eh
	ADD	A,C
	ADD	A,C
	ADD	A,C
	CALL	L0x3153
	POP	AF
	LD	C,A
	JR	L0x3106
L0x30B0:
	SUB	40h
	PUSH	AF
	LD	B,0B3h
	CP	040h
	JR	C,L0x30C1
	LD	B,0B6h
	CP	080h
	JR	C,L0x30C1
	LD	B,0B9h
L0x30C1:
	LD	A,B
	CALL	L0x3153
	POP	AF
	AND	03Fh
	CALL	L0x31AC
	PUSH	AF
	LD	A,C
	CALL	hex2ascii
	CALL	addcomma
	POP	AF
	LD	C,A
	JR	L0x3106
L0x30D7:
	LD	HL,(PERFROM)
	INC	HL
	LD	A,(HL)
	CP	0CBh
	JR	Z,L0x30EA
	CP	0BFh
	DEC	HL
	RET	NC
	CP	040h
	JR	NC,L0x30ED
	AND	A
	RET

L0x30EA:
	SET	7,C
	INC	HL
L0x30ED:
	INC	HL
	LD	A,(HL)
	PUSH	AF
	LD	B,004h
	LD	A,C
	LD	(DISFLAG),A
	BIT	7,C
	JR	NZ,L0x30FB
	DEC	B
L0x30FB:
	CALL	L0x31FF
	POP	AF
	BIT	7,C
	JR	NZ,L0x3096
	JP	L0x305B


L0x3106:
	LD	A,C
L0x3107:
	CP	006h
	JR	Z,L0x3112		; is it HL/IX/IY?
L0x310B:
	LD	A,001h			; one register
L0x310D:
	LD	HL,TBL_REG		; set start point for lookup
	JR	L0x3145

L0x3112:
	LD	A,(DISFLAG)
	OR	A
	JR	NZ,L0x311E
	LD	C,008h
	LD	A,004h
	JR	L0x310D
L0x311E:
	PUSH	AF
	RRA
	LD	C,00Ch
	JR	C,L0x3126
	LD	C,013h
L0x3126:
	LD	A,007h
	CALL	L0x310D
	POP	AF
	RLA
	LD	DE,(PERFROM)
	JR	NC,L0x3134
	DEC	DE
L0x3134:
	LD	A,(DISEND)
	SUB	003h
	LD	(DISEND),A
	LD	A,(DE)
	CALL	atoascii
	INC	HL
	LD	(DISEND),HL
	RET

;copy ASCII to buffer based on BC as an index; B is set externally

L0x3145:
	LD	DE,(DISEND)
	ADD	HL,BC
	LD	C,A
	LDIR
	LD	(DISEND),DE
	SCF
	RET

; finds the text in the TBL_OPS table and outputs it
; A register +82h points to table entry required
; so subtract 82h to get the right location (Leftover quirk of original code)


L0x3153:
	LD HL,TBL_OPS

	sub 82h				; realign A
	ld e,a				; and add offset to HL
	ld d,0
	add hl,de

;	ld	H,37h			; high byte of table location
;	LD	L,A
;	LD	DE,0FE9Eh
;	ADD	HL,DE
	EX	DE,HL
	JR	asciilookup
L0x315D:
	LD	HL,DISMID
	LD	(DISEND),HL
	RET
addcomma:
	LD	BC,00006h
	JR	L0x310B

; ASCII lookup with DE as the table address
; copies table entry to buffer

asciilookup:
	LD	HL,(DISEND)
	LD	A,(DE)
	LD	(HL),A
	RES	7,(HL)
	INC	HL
	LD	(DISEND),HL
	INC	DE
	OR	A
	JP	M,addspc1
	JR	asciilookup

L0x317B:
	LD	A,C
	CP	004h
	JR	C,L0x3182
	OR	A
	RET
L0x3182:
	CP	002h
	JR	NZ,L0x3188
	DEC	A
	RET
L0x3188:
	XOR	A
	RET

; three display conversion routines for A register in HEX, to ASCII

hltoascii:
	PUSH	HL
	LD	A,H
	CALL	atoascii
	POP	HL
	LD	A,L
atoascii:
	PUSH	AF
	RRA
	RRA
	RRA
	RRA
	CALL	hex2ascii
	POP	AF
hex2ascii:
	AND	0Fh
	ADD	A,090h
	DAA
	ADC	A,040h
	DAA
	LD	HL,(DISEND)
	LD	(HL),A
	INC	HL
	LD	(DISEND),HL
	SCF
	RET

L0x31AC:
	PUSH	AF
	AND	038h
	RRA
	RRA
	RRA
	LD	C,A
	POP	AF
	AND	007h
	RET

addspc1:
	LD	B,1

; inserts a space into the display buffer

addspc:
	LD	A,020h
	LD	HL,(DISEND)
L0x31BE:
	LD	(HL),A
	INC	HL
	DJNZ	L0x31BE
	LD	(DISEND),HL
	RET

; convert current opcode to ASCII (HEX digits) and add to buffer
; B = number of opcodes to process


L0x31C6:
	LD	DE,(PERFROM)

L0x31CA:
	PUSH	BC
	LD	A,(DE)			; get opcode
	PUSH	AF
	CALL	atoascii		; convert it to ascii sctring
	CALL	addspc1			; throw in a space character between opcodes
	POP	AF
	INC	DE
	POP	BC
	DJNZ	L0x31CA			; loop till done
	DEC	DE
	LD	(PERFROM),DE
	RET
L0x31DE:
	LD	B,002h
	CALL	L0x31C6
	CALL	L0x315D
	JP	L0x3096
L0x31E9:
	AND	0CFh
	CP	001h
	JR	NZ,L0x3236
	CALL	L0x31FD
	CALL	L0x3205
	CALL	L0x320A
	CALL	addcomma
	JR	L0x3221
L0x31FD:
	LD	B,003h
L0x31FF:
	CALL	L0x31C6
	JP	L0x315D
L0x3205:
	LD	A,083h
	JP	L0x3153
L0x320A:
	LD	A,C
L0x320B:
	PUSH	AF
	AND	030h
	CALL	L0x35A6
	ADD	A,01Ah
	LD	B,000h
	LD	C,A
	LD	A,002h
	LD	HL,TBL_REG
	CALL	L0x3145
	POP	AF
	LD	C,A
	RET

L0x3221:
	LD	HL,(PERFROM)
	LD	A,(HL)
	PUSH	HL
	CALL	atoascii
	POP	HL
	DEC	HL
	LD	A,(HL)
	PUSH	HL
	CALL	atoascii
	POP	HL
	INC	HL
	LD	(PERFROM),HL
	RET

L0x3236:
	AND	0C7h
	CP	006h
	JR	NZ,L0x3254
	LD	B,002h
	CALL	L0x32BE
	LD	A,C
	CALL	L0x31AC
	LD	B,000h
	CALL	L0x35E7
	CALL	addcomma
L0x324D:
	LD	HL,(PERFROM)
	LD	A,(HL)
	JP	atoascii
L0x3254:
	LD	A,C
	PUSH	AF
	AND	0EFh
	CP	00Ah
	JR	NZ,L0x327B
	LD	B,001h
	CALL	L0x32BE
	LD	A,C
	LD	BC,00007h
	CALL	L0x310B
	CALL	addcomma
L0x326B:
	LD	BC,0008h
	CALL	L0x310B
	POP	AF
	CALL	L0x320B
	LD	BC,0000Bh
	JP	L0x310B
L0x327B:
	CP	002h
	JR	NZ,L0x3290
	LD	B,001h
	CALL	L0x32BE
	CALL	L0x3603
L0x3287:
	CALL	addcomma
	LD	BC,00007h
	JP	L0x310B
L0x3290:
	CP	022h
	JR	NZ,L0x32C4
	LD	B,003h
	CALL	L0x32BE
	CALL	L0x32AF
	CALL	addcomma
	POP	AF
L0x32A0:
	BIT	4,A
	JR	NZ,L0x32A7
L0x32A4:
	JP	L0x35CD
L0x32A7:
	LD	A,001h
	LD	BC,00007h
	JP	L0x310D
L0x32AF:
	LD	BC,00008h
	CALL	L0x310B
	CALL	L0x3221
	LD	BC,0000Bh
	JP	L0x310B
L0x32BE:
	CALL	L0x31FF
	JP	L0x3205
L0x32C4:
	CP	02Ah
	JR	NZ,L0x32D6
	LD	B,003h
	CALL	L0x32BE
	POP	AF
	CALL	L0x32A0
	CALL	addcomma
	JR	L0x32AF
L0x32D6:
	AND	0CFh
	CP	003h
	JR	NZ,L0x32EB
	CALL	L0x32E3
	POP	AF
	JP	L0x320B
L0x32E3:
	CALL	L0x3055
	LD	A,0BCh
L0x32E8:
	JP	L0x3153
L0x32EB:
	CP	00Bh
	JR	NZ,L0x32FB
	CALL	L0x32F6
	POP	AF
	JP	L0x320B
L0x32F6:
	LD	A,0BFh
	JP	L0x3511
L0x32FB:
	AND	0C7h
	CP	004h
	JR	NZ,L0x330B
	CALL	L0x32E3
	POP	AF
L0x3305:
	CALL	L0x31AC
	JP	L0x3106
L0x330B:
	CP	005h
	JR	NZ,L0x3315
	CALL	L0x32F6
	POP	AF
	JR	L0x3305
L0x3315:
	LD	A,C
	AND	0CFh
	CP	009h
	JR	NZ,L0x332B
	LD	A,086h
	CALL	L0x3511
	CALL	L0x32A4
	CALL	addcomma
	POP	AF
	JP	L0x320B
L0x332B:
	POP	AF
	CP	010h
	JR	NZ,L0x334B
	LD	A,0D9h
L0x3332:
	PUSH	AF
	LD	B,002h
	CALL	L0x31FF
	POP	AF
	CALL	L0x3153
	JP	L0x35F3
L0x334B:
	CP	018h
	JR	NZ,L0x3353
	LD	A,0D5h
	JR	L0x3332
L0x3353:					; JR Z and JR C checks
	LD	A,C
	AND	0C7h
	OR	A
	JR	NZ,L0x3377
	LD	A,C
	PUSH	AF
	LD	B,002h
	CALL	L0x31FF
	LD	A,0D5h
	CALL	L0x3153
	POP	AF
	CALL	L0x336C
	call fixcomma
	JP	L0x35F3
fixcomma:					; comma fix for JP/JR <flag>, xxx
	push hl
	push af
	ld hl,(DISEND)
	dec hl
	LD a,02ch				; ","
	ld (hl),a
	inc hl
	LD a,020h				; " "
	ld (hl),a
	inc hl
	ld (DISEND),hl
	pop af
	pop hl
	ret

L0x336C:
	AND	018h
L0x336E:
	RRA
	RRA
	RRA
	ADD	A,A
	ADD	A,0DDh
	JP	L0x3153
L0x3377:
	LD	A,C
	CP	0C3h
	JR	Z,L0x3391
	CP	0CDh
	JR	Z,L0x338D
	jr L0x33A7
L0x3389:
	AND	038h
	JR	L0x336E
L0x338D:
	LD	A,0C8h
	JR	L0x3393
L0x3391:
	LD	A,0CCh
L0x3393:
	LD	B,003h
	PUSH	AF
	CALL	L0x31FF
	POP	AF
	CP	0A6h
	JR	NZ,L0x33A1
	JP	L0x3153
L0x33A1:
	CALL	L0x32E8
	JP	L0x3221
L0x33A7:
	AND	0C7h
	CP	0C0h
	JR	NZ,L0x33C5
	LD	A,0C5h
	LD	B,001h
	CALL	L0x33B8
	DEC	HL
	LD	(HL),020h
	RET
L0x33B8:
	PUSH	BC
	PUSH	AF
	CALL	L0x31FF
	POP	AF
	CALL	L0x32E8
	POP	BC
	LD	A,C
	JR	L0x3389
L0x33C5:
	LD	A,C
	AND	0C7h
	CP	0C4h
	JR	NZ,L0x33D6
	LD	A,0C8h

L0x33CE:					; CALL & JP NZ/NC/PO/P output
	LD	B,003h
	CALL	L0x33B8
	call fixcomma
	JP	L0x3221
L0x33D6:
	CP	0C2h
	JR	NZ,L0x33DE
	LD	A,0CCh
	JR	L0x33CE

L0x33DE:
	LD	A,C
	AND	0CFh
	CP	0C1h
	JR	NZ,L0x33E9
	LD	A,0CEh
	JR	L0x33EE
L0x33E9:
	CP	0C5h
	RET	NZ
	LD	A,0D1h
L0x33EE:
	PUSH	BC
	PUSH	AF
	CALL	L0x3055
	POP	AF
	CALL	L0x32E8
	POP	BC
	LD	A,C
	CP	0F1h
	JR	Z,L0x3401
	CP	0F5h
	jp nz,L0x320B
L0x3401:
	LD	(HL),041h
	INC	HL
	LD	(HL),046h
	INC	HL
	LD	(DISEND),HL
	RET
miscop:
	AND	0C7h
	CP	0C7h
	JR	NZ,L0x3421
	LD	A,C
	PUSH	AF
	LD	A,0C2h
	CALL	L0x3511
	POP	AF
	AND	038h
	JP	atoascii
L0x3421:
	AND	0C6h
	CP	0C6h
	LD	A,C
	JR	NZ,L0x3440
	XOR	A
	CALL	L0x34F7
	LD	A,C
	PUSH	BC
	CALL	L0x31AC
	LD	A,086h
	ADD	A,C
	ADD	A,C
	ADD	A,C
	CALL	L0x3153
	CALL	L0x3086
	POP	BC
	JP	L0x324D
L0x3440:
	CP	0EDh
	JP	NZ,L0x34E8
	LD	HL,(PERFROM)
	INC	HL
	LD	A,(HL)
	LD	C,A
	AND	0C7h
	CP	043h
	JR	NZ,L0x3471
	LD	B,004h
	CALL	L0x31FF
	CALL	L0x3205
	BIT	3,C
	JR	NZ,L0x3468
	PUSH	BC
	CALL	L0x32AF
	CALL	addcomma
	POP	BC
	JP	L0x320A
L0x3468:
	CALL	L0x320A
	CALL	addcomma
	JP	L0x32AF
L0x3471:
	LD	B,002h
	JR	L0x34DF
L0x3475:
	LD	A,C
	AND	0C7h
	CP	040h
	JR	NZ,L0x3496
	PUSH	BC
	LD	A,0F0h
	CALL	L0x3153
	POP	BC
	LD	A,C
	CALL	L0x31AC
	LD	A,C
	CALL	L0x310B
	CALL	addcomma
L0x348E:
	LD	BC,00022h
	LD	A,003h
	JP	L0x310D
L0x3496:
	CP	041h
	JR	NZ,L0x34AF
	PUSH	BC
	LD	A,0EDh
	CALL	L0x3153
	CALL	L0x348E
	CALL	addcomma
	POP	BC
	LD	A,C
	CALL	L0x31AC
	LD	A,C
	JP	L0x310B
L0x34AF:
	CP	042h
	JR	NZ,L0x34E8
	PUSH	BC
	LD	A,C
	BIT	3,A
	JR	NZ,L0x34C0
	LD	A,08Fh
	CALL	L0x3153
	JR	L0x34C5
L0x34C0:
	LD	A,089h
	CALL	L0x3153
L0x34C5:
	CALL	L0x32A4
	CALL	addcomma
	POP	BC
	JP	L0x320A
L0x34CF:
	LD	B,01Ch
	LD	HL,(PERFROM)
	INC	HL
	LD	A,(HL)
	LD	HL,TBL_EXC
	LD	B,01Ch
	LD	D,002h
	JR	L0x3522
L0x34DF:
	AND	084h
	JR	NZ,L0x34CF
	CALL	L0x31FF
	JR	L0x3475
L0x34E8:
	CP	0D3h
	JR	NZ,L0x3502
	LD	A,0EDh
	CALL	L0x34F7
	CALL	L0x34FB
	JP	L0x3287
L0x34F7:
	LD	B,002h
	JR	L0x3513

L0x34FB:					; put brackets around OUT (xx),A & IN A,(xx)
	ld hl,(DISEND)
	LD a,028h				; "("
	ld (hl),a
	inc hl
	ld (DISEND),hl
	call L0x324D
	ld hl,(DISEND)
	LD a,029h				; ")"
	ld (hl),a
	inc hl
	ld (DISEND),hl
	ret

L0x3502:
	CP	0DBh
	JR	NZ,L0x354B
	LD	A,0F0h
	CALL	L0x34F7
	LD	C,00h
;	PUSH	BC
;	JP	L0x3439
	call L0x3086
	jp L0x34FB


L0x3511:
	LD	B,01h
L0x3513:
	PUSH	AF
	CALL	L0x31FF
	POP	AF
	OR	A
	JP	NZ,L0x3153
	RET
onebyt:
	LD	B,13h				; 13h bytes of opcodes to test
	LD	HL,TBL_OBT
L0x3522:
	CP	(HL)				; check for a match in one byte table
	JR	Z,L0x352F
L0x3525:
	INC	HL
	BIT	7,(HL)
	JR	Z,L0x3525
	INC	HL
	DJNZ	L0x3522
	SCF
	RET
L0x352F:
	PUSH	HL				; got a match to one byte opcode
	LD	B,D				; d = how many opcodes to display
	CALL	L0x31FF				; call display opcode routine
	POP	HL
	LD	DE,(DISEND)
L0x3539:
	INC	HL				; this loop copies the opcode text
	LD	A,(HL)				; to buffer line 2
	LD	(DE),A
	INC	DE
	BIT	7,A
	JR	Z,L0x3539
	EX	DE,HL
	DEC	HL
	RES	7,(HL)
	INC	HL
	LD	(DISEND),HL
	OR	A
	RET

L0x354B:
	LD	HL,(DISEND)
	CP	0DDh
	JR	NZ,L0x3559
	LD	(HL),044h
	INC	HL
	LD	A,011h
	JR	L0x3562
L0x3559:
	CP	0FDh
	JR	NZ,L0x3590
	LD	(HL),046h
	INC	HL
	LD	A,022h
L0x3562:
	LD	(DISFLAG),A
	LD	(HL),044h
	INC	HL
	INC	HL
	LD	(DISEND),HL
	LD	HL,(PERFROM)
	INC	HL
	LD	C,(HL)
	LD	A,(HL)
	LD	(PERFROM),HL
	CP	036h
	PUSH	HL
	CALL Z,L0x31FD
	LD	A,C
	AND	0FEh
	CP	034h
	LD	B,02h
	CALL Z,L0x31FF
	POP	HL
	LD	A,(HL)
	CP	0E9h
	JR	Z,L0x358D
	CP	0E3h
L0x358D:
	JR	Z,L0x360A
	LD	A,C
L0x3590:
	JP	L0x31E9
L0x3593:
	LD	HL,(PERFROM)
	DEC	HL
	DEC	HL
	LD	(PERFROM),HL
	CALL	L0x3106

L0x35A6:
	RRA
	RRA
	RRA
	CP	04h
	RET	NZ
	LD	A,(DISFLAG)
	RRCA
	JR	NC,L0x35B5
	LD	A,0F3h
	RET
L0x35B5:
	RRCA
	JR	NC,L0x35BB
	LD	A,0FAh
	RET
L0x35BB:
	LD	A,004h
	RET
	LD	BC,00008h
	CALL	L0x310B
	CALL	L0x35CD
	LD	BC,0000Bh
	JP	L0x310B
L0x35CD:
	LD	A,(DISFLAG)
	LD	B,000h
	RRCA
	JR	NC,L0x35D9
	LD	C,00Dh
	JR	L0x35E2
L0x35D9:
	RRCA
	JR	NC,L0x35E0
	LD	C,014h
	JR	L0x35E2
L0x35E0:
	LD	C,004h
L0x35E2:
	LD	A,002h
	JP	L0x310D
L0x35E7:
	LD	A,(DISFLAG)
	RRCA
L0x35EB:
	JR	C,L0x3593
	RRCA
	JR	C,L0x35EB
	JP	L0x3106
L0x35F3:
	LD	HL,(PERFROM)
	LD	E,(HL)
	XOR	A
	BIT	7,E
	JR	Z,L0x35FD
	CPL
L0x35FD:
	LD	D,A
	INC	HL
	ADD	HL,DE
	JP	hltoascii
L0x3603:
	POP	HL
	POP	AF
	PUSH	HL
	PUSH	AF
	JP	L0x326B
L0x360A:
	LD	A,C
	CALL	onebyt
L0x360E:
	DEC	HL
	LD	A,(HL)
	CP	048h
	JR	NZ,L0x360E
	LD	(HL),049h
	INC	HL
	LD	(HL),058h
	LD	A,(DISFLAG)
	RRCA
	RET	C
	INC	(HL)
	RET


;START OF Loopup tables

TBL_OPS:
	.DB	 049h 			;I 82 < WHERE "82H" IS THE INDEX HERE
	.DB	 04Ch,0C4h,000h 	;LD 83
	.DB	 041h,044h,0C4h 	;ADD 86
	.DB	 041h,044h,0C3h 	;ADC 89
	.DB	 053h,055h,0C2h 	;SUB 8C
	.DB	 053h,042h,0C3h 	;SBC 8F
	.DB	 041h,04Eh,0C4h 	;AND 92
	.DB	 058h,04Fh,0D2h 	;XOR 95
	.DB	 04Fh,0D2h,000h 	;OR 98
	.DB	 043h,0D0h,000h 	;CP 9B
	.DB	 052h,04Ch,0C3h 	;RLC 9E
	.DB	 052h,052h,0C3h 	;RRC A1
	.DB	 052h,0CCh,000h 	;RL A4
	.DB	 052h,0D2h,000h 	;RR A7
	.DB	 053h,04Ch,0C1h 	;SLA AA
	.DB	 053h,052h,0C1h 	;SRA AD
	.DB	 053h,052h,0CCh 	;SRL B0
	.DB	 042h,049h,0D4h 	;BIT B3
	.DB	 052h,045h,0D3h 	;RES B6
	.DB	 053h,045h,0D4h 	;SET B9
	.DB	 049h,04Eh,0C3h 	;INC BC
	.DB	 044h,045h,0C3h 	;DEC BF
	.DB	 052h,053h,0D4h 	;RST C2
	.DB	 052h,045h,0D4h 	;RET C5
	.DB	 043h,041h,04Ch,0CCh 	;CALL C8
	.DB	 04Ah,0D0h 		;JP CC
	.DB	 050h,04Fh,0D0h		;POP CE
	.DB	 050h,055h,053h,0C8h	;PUSH D1
	.DB	 04Ah,0D2h 		;JR D5
	.DB	 045h,0D8h 		;EX D7
	.DB	 044h,04Ah,04Eh,0DAh	;DJNZ D9

TBL_FLG:
	.DB	 04Eh,0DAh 		;NZ DD
	.DB	 0DAh,020h 		;Z_ DF
	.DB	 04Eh,0C3h 		;NC E1
	.DB	 0C3h,000h 		;C_E3
	.DB	 050h,0CFh 		;PO E5
	.DB	 050h,0C5h 		;PE E7
	.DB	 0D0h,000h 		;P_ E9
	.DB	 0CDh,000h 		;M_ EB
	.DB	 04Fh,055h,0D4h 	;OUT ED
	.DB	 049h,0CEh 		;IN F0

TBL_OBT:
	.DB	 000h,04Eh,04Fh,0D0h 	;NOP
	.DB	 007h,052h,04Ch,043h,0C1h ;RLCA
	.DB	 008h,045h,058h,020h,041h,046h,02Ch,041h,046h,0A7h ;EX AF,AF'
	.DB	 00Fh,052h,052h,043h,0C1h ;RRCA
	.DB	 017h,052h,04Ch,0C1h 	;RLA
	.DB	 01Fh,052h,052h,0C1h 	;RRA
	.DB	 027h,044h,041h,0C1h 	;DAA
	.DB	 02Fh,043h,050h,0CCh 	;CPL
	.DB	 037h,053h,043h,0C6h 	;SCF
	.DB	 03Fh,043h,043h,0C6h 	;CCF
	.DB	 076h,048h,041h,04Ch,0D4h ;HALT
	.DB	 0C9h,052h,045h,0D4h 	;RET
	.DB	 0D9h,045h,058h,0D8h 	;EXX
	.DB	 0E3h,045h,058h,020h,028h,053h,050h,029h,02Ch,048h,0CCh ;EX (SP),HL
	.DB	 0E9h,04Ah,050h,020h,028h,048h,04Ch,0A9h ;JP (HL)
	.DB	 0EBh,045h,058h,020h,044h,045h,02Ch,048h,0CCh ;EX DE,HL
	.DB	 0F3h,044h,0C9h 	;DI
	.DB	 0F9h,04Ch,044h,020h,053h,050h,02Ch,048h,0CCh ;LD SP,HL
	.DB	 0FBh,045h,0C9h 	;EI

TBL_EXC:
	.DB	 044h,04Eh,045h,0C7h ;NEG
	.DB	 045h,052h,045h,054h,0CEh ;RETN
	.DB	 046h,049h,04Dh,020h,0B0h ;IM 0
	.DB	 047h,04Ch,044h,020h,049h,02Ch,0C1h ;LD I,A
	.DB	 04Dh,052h,045h,054h,0C9h ;RETI
	.DB	 04Fh,04Ch,044h,020h,052h,02Ch,0C1h ;LD R,A
	.DB	 056h,049h,04Dh,020h,0B1h ;IM 1
	.DB	 057h,04Ch,044h,020h,041h,02Ch,0C9h ;LD A,I
	.DB	 05Eh,049h,04Dh,020h,0B2h ;IM 2
	.DB	 05Fh,04Ch,044h,020h,041h,02Ch,0D2h ;LD A,R
	.DB	 067h,052h,052h,0C4h ;RRD
	.DB	 06Fh,052h,04Ch,0C4h ;RLD
	.DB	 0A0h,04Ch,044h,0C9h ;LDI
	.DB	 0A1h,043h,050h,0C9h ;CPI
	.DB	 0A2h,049h,04Eh,0C9h ;INI
	.DB	 0A3h,04Fh,055h,054h,0C9h ;OUTI
	.DB	 0A8h,04Ch,044h,0C4h ;LDD
	.DB	 0A9h,043h,050h,0C4h ;CPD
	.DB	 0AAh,049h,04Eh,0C4h ;IND
	.DB	 0ABh,04Fh,055h,054h,0C4h ;OUTD
	.DB	 0B0h,04Ch,044h,049h,0D2h ;LDIR
	.DB	 0B1h,043h,050h,049h,0D2h ;CPIR
	.DB	 0B2h,049h,04Eh,049h,0D2h ;INIR
	.DB	 0B3h,04Fh,054h,049h,0D2h ;OTIR
	.DB	 0B8h,04Ch,044h,044h,0D2h ;LDDR
	.DB	 0B9h,043h,050h,044h,0D2h ;CPDR
	.DB	 0BAh,049h,04Eh,044h,0D2h ;INDR
	.DB	 0BBh,04Fh,054h,044h,0D2h ;OTDR

TBL_REG:
	.DB	 042h,043h 		;BC
	.DB	 044h,045h 		;DE
	.DB	 048h,04Ch 		;HL
	.DB	 02Ch,041h 		;,A
	.DB	 028h,048h,04Ch,029h 	;(HL)
	.DB	 028h,049h,058h,02Bh,020h,01Fh,029h ;(IX+__)
	.DB	 028h,049h,059h,02Bh,020h,01Fh,029h ;(IY+__)

TBL_SREG:
	.DB	 042h 			;B
	.DB	 043h 			;C
	.DB	 044h 			;D
	.DB	 045h 			;E
	.DB	 048h 			;H
	.DB	 04Ch 			;L
	.DB	 053h 			;S
	.DB	 050h 			;P
	.DB	 028h,043h,029h 	;(C)

; end of Jim's disassembler


; 6850 ACIA registers
;----------------------
CONTROL         .EQU      $80   ;(write)
STATUS          .EQU      $80   ;(read)
TDR             .EQU      $81   ;(write)
RDR             .EQU      $81   ;(read)
;
; control register bits
;----------------------
;
;clock divisor
;
MRESET  .EQU  $03        ;master reset the ACIA
DIV0    .EQU  $00        ;CLOCK/1
DIV16   .EQU  $01        ;CLOCK/16
DIV64   .EQU  $02        ;CLOCK/64
;
; format select
;
F7E2    .EQU   $00        ;7 data bits, EVEN parity, 2 stop bits (1+7+1+2= 11 bits)
F7O2    .EQU   $04        ;7 data bits, ODD parity, 2 stop bits (1+7+1+2= 11 bits)
F7E1    .EQU   $08        ;7 data bits, EVEN parity, 1 stop bit (1+7+1+1= 10 bits)
F7O1    .EQU   $0C        ;7 data bits, ODD parity, 1 stop bit (1+7+1+1= 10 bits)
F8N2    .EQU   $10        ;8 data bits, NO parity, 2 stop bits (1+8+0+2= 11 bits)
F8N1    .EQU   $14        ;8 data bits, NO parity, 1 stop bit (1+8+0+1= 10 bits)
F8E1    .EQU   $18        ;8 data bits, EVEN parity, 1 stop bit (1+8+1+1= 11 bits)
F8O1    .EQU   $1C        ;8 data bits, ODD parity,1 stop bit (1+8+1+1= 11 bits)
;
; transmitter control
;
RTSLID .EQU   $00        ;RTS LOW, transmit interrupt disabled
RTSLIE .EQU   $20        ;RTS LOW, transmit interrupt enabled
RTSHID .EQU   $40        ;RTS HIGH, transmit interrupt disabled
RTSLIDB .EQU  $60        ;RTS LOW, transmit interrupt disabled and 'break' transmitted
;
; receiver interrupt
;
RIE    .EQU   $80        ;receiver interrupt enabled
;
; status register bits
;---------------------
RDRF   .EQU   0          ;receive data register full
TDRE   .EQU   1          ;transmit data register empty
DCD    .EQU   2          ;data carrier detect
CTS    .EQU   3          ;clear to send
FE     .EQU   4          ;framing error
OVRN   .EQU   5          ;overrun
PE     .EQU   6          ;parity error
IRQ    .EQU   7          ;interrupt request



smode	.db	0



stx:	ld b,a			; serial Tx

	ld a,(smode)
	cp 1
	jr nz,bangtx

	ld a,b			; 6850
	jp TxChar


bangtx:	ld a,b			; bang
	ld c,TXDATA
	rst 30h

	ret



srx:	ld a,(smode)		; Serial Rx
	cp 1
	jr nz,bangrx

	jp RxChar		; 6850

bangrx:
	ld c,RXDATA		; bang
	rst 30h
	ret




prtinl:	POP	HL	;RETURN ADDRESS IS START OF STRING
lsz:	LD	A,(HL)	;GET CHARACTER
	CALL	stx	;AND SEND IT
	INC	HL	;POINT TO THE NEXT
	OR	A	;CHARACTER AND
	JR	NZ,lsz	;RETURN IF CHAR = 0
	JP	(HL)	;RETURN TO LOCATION AFTER 0 TERMINATOR

;
;
;

smsg:	PUSH	BC
	PUSH	HL
	PUSH	AF
	LD	B,255	;255 CHARS MAX
smsg1:	LD	A,(HL)	;GET THE CHAR
	CP	00H	;ZERO TERMINATOR?
	JR	Z,smsg2	;FOUND A ZERO TERMINATOR, EXIT
	CALL	stx	;TRANSMIT THE CHAR
	INC	HL
	DJNZ	smsg1	;255 CHARS MAX!

smsg2:	POP	AF
	POP	HL
	POP	BC
	RET



Sinit:	ld    a,MRESET
	out   (CONTROL),a           ;reset the ACIA
	ld     a,RTSLID+F8N2+DIV64
	out   (CONTROL),a           ;initialise ACIA  8 bit word, No parity 2 stop divide by 64 for 115200 baud
	ret

;
; transmit a character in a
;--------------------------
TxChar:  ld    b,a                   ;save the character  for later
TxChar1: in    a,(STATUS)            ;get the ACIA status
         bit   TDRE,a                ;is the TDRE bit high?
         jr    z,TxChar1             ;no, the TDR is not empty
         ld    a,b                   ;yes, get the character
         out   (TDR),a               ;and put it in the TDR
         ret
;
; receive  a character in a
;---------------------------------
RxChar:  in    a,(STATUS)         ;get the ACIA status
         bit   RDRF,a             ;is the RDRF bit high?
         jr    z,RxChar           ;no, the RDR is empty
         in    a,(RDR)            ;yes, read the received char
         ret



	.end

