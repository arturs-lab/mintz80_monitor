

		org $2400

start:	ld a,00
		out ($db),a
		ld hl,$6000
		ld de,$a000
		ld bc,$2000
		ldir
		ld a,01
		out ($db),a
		jp $a000

endprog	equ $

;	output_bin "run_a000.bin",start,endprog-start
	output_intel "run_a000.hex",start,endprog-start
	output_list "run_a000.lst"

