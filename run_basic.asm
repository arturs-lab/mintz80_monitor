

		org $8000

start:	di
		ld a,03		; switch first 2 banks to RAM
		out ($d8),a
		out ($d9),a
		ld a,02		; switch next two banks to EEPROM2,3
		out ($da),a
		out ($db),a
		ld hl,$4000
		ld de,$0000
		ld bc,$4000
		ldir
		ld a,01		; switch bank 2,3 to RAM
		out ($da),a
		out ($db),a
hot:		di			; have to do it here again in case we come in through here and miss first time
		ld a,03		; switch first 2 banks to RAM. We can enter here if basic was already loaded
		out ($d8),a
		out ($d9),a
		jp $0000

endprog	equ $

;	output_bin "run_basic.bin",start,endprog-start
	output_intel "run_basic.hex",start,endprog-start
	output_list "run_basic.lst"

