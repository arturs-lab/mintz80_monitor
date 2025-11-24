

		org $2400

start:	ld a,02
		out ($da),a
		ld hl,$4000
		ld de,$a000
		ld bc,$2000
		ldir
		ld a,01
		out ($da),a
		jp $a000

endprog	equ $

;	output_bin "run_a000.bin",start,endprog-start
	output_intel "run_a000.hex",start,endprog-start
	output_list "run_a000.lst"

