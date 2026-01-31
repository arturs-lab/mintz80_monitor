
	org $2400

start:	equ $
if MACHINE="MintZ80"
		di
endif
		ld hl,$ff00		; start at top of RAM
		ld bc,0
		ld a,h
tst2:		cp $3f			; did we reach bottom of test? stop testing before $2000-#3fff block because monitor uses that
		jr z,tstup			; yes, test from here up

		rlc a
		rlc a
		rlc a
if MACHINE="MintZ80"
		add a,-$20
endif
		ld c,a
		ld a,$0f			; start with highest page
tst1:		out (c),a			; select that page in tested slot and
		ld (hl),a			; store that page number in RAM of that slot
		dec a
		dec a
		cp $ff
		jr nz,tst1			; repeat for all pages
		ld a,-$20			; go to next slot
		add h
		ld h,a
		jr tst2
		

tstup:	call jCON_PRT_NL
		ld a,$20		; now that all pages of all tested slots are filled
		add a,h		; account for last subtraction
		ld h,a
		cp $1f		; did we reach last slot?
		jr nz,tst5

if MACHINE="MintZ80"
		ei
endif
		ret	; return if so
		
tst5:		call jCON_PRINTHWORD ; print tested slot address
		call jCON_PRT_STR_SP	; print banner
zoWarnFlow = false
		db ": ",0
zoWarnFlow = true

		rlc a
		rlc a
		rlc a
if MACHINE="MintZ80"
		add a,-$20
endif
		ld c,a
		ld a,$1			; start with lowest bank, $01
tst3:		out (c),a			; select bank in A
		call jCON_PRINTHBYTE
		cp (hl)			; compare with previously stored number
		jr nz,tst4			; jump if no match, this bank does not exist
		call jCON_PRT_STR_SP	; print banner
zoWarnFlow = false
		db "* ",0
zoWarnFlow = true
		add $02			; we started at 1, RAM banks are odd numbered
		cp $10			; $0f is last bank we want to test
		jr c,tst3			; havent passed it yet
		
tst4:		jr tstup

		ld hl,$3f00
		jr tstup

		include "jump.inc"

endprog	equ $

	output_bin "ramtest.bin",start,endprog-start		; The binary file
	output_intel "ramtest.hex",start,endprog-start		; The binary file
	output_list "ramtest.lst"
