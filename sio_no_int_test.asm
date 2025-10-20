; ------------------------------------------------------------------------------
; LM80C - SERIAL COMM test
; ------------------------------------------------------------------------------
; This code is intended to be used with LM80C Z80-based computer
; designed by Leonardo Miliani. More info at
; www DOT leonardomiliani DOT com
; ------------------------------------------------------------------------------
; Written by Leonardo Miliani
; Based on code samples from "How To Program the Z80 Periphery Tutorial" by Mario Blunk
; Edited with Atom Editor
; Compiled with ZASM assembler 4.2.4 on MacOS
; ------------------------------------------------------------------------------
; Released under the terms of GPL v.3 or any successive release
; ------------------------------------------------------------------------------
; Revisions:
; 0.1 - 20190209 - First version
; 0.2 - 20190219 - Second version - almost working
; 1.0 - 20190220 - Stable version
; ------------------------------------------------------------------------------

		include "jump.inc"

; ADDRESS DECODING
; A4/A5
; 0000xxxx : PIO
; 0001xxxx : CTC
; 0010xxxx : SIO

RAMCELL     equ 0x3080

;-------------------------------------------------------------------------------
; MAIN
;-------------------------------------------------------------------------------

        org $4000
main:		call setPIO         ; set PIO
;        call jCTC_INIT_ALL     ; set CTC
;        call jSIO_INIT         ; set SIO
;		call jSIO_A_INT_SET	; initialize SIOA interrupts
;		call jSIO_A_EI		; more interrupt code

        ld a,$20      ; load initial LED pattern into RAM
        ld (RAMCELL),a

		ld b,$60

; print welcome string
		ld hl,hlo
prt_hlo:	ld a,(hl)
		cp 0
		jr z, main_loop
		call jSIOA_TX
		inc hl
		jr prt_hlo

main_loop:	ld a,(RAMCELL)      ; load the byte from RAM
        inc a
		and a,$7f
        cp $7f
        jr nz,storeByte
        ld a,$20

storeByte:	out (PIO_DB),a      ; send the pattern to the PIO
        ld (RAMCELL),a      ; write the new pattern into RAM for later reload

        ld c,0xff           ; a little delay
        call delay
        djnz main_loop           ; repeat 256 times

		di
		sub a
		ret

hlo:	db $0d,$0a,"Hellorld!",$0d,$0a,$0

;-------------------------------------------------------------------------------
; Subroutines
;-------------------------------------------------------------------------------
setPIO:
;program the PIO
        ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CB),a
        ld a,00000000b      ; set pins of port A to OUTPUT
        out (PIO_CB),a

        ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CA),a
        ld a,00000000b      ; set pins of port A to OUTPUT
        out (PIO_CA),a

        ret

;-------------------------------------------------
; routine to add a programmable delay (set by value stored in C)
delay:	push bc
loop1:	ld b,0xff
loop2:	djnz loop2
		call jSIOA_RX_CHK
		jr z,loop3
		ld (RAMCELL+1),a
		call jSIOA_TX
loop3:  dec c
        jr nz, loop1
        pop bc
        ret

reconfig:	ld a,00000100b      ; write into WR0: select WR4
	out (SIO_CA),a
	ld a,01000100b      ; write into WR4: presc. 16x, 1 stop bit, no parity
	out (SIO_CA),a
;	ld a,$1e			; $1e = 4800 baud
;	ld a,$3c			; $3c = 2400 baud
	ld a,$78			; $78 = 1200 baud
	ld (CTC_CH2_TC),a
	ld (CTC_CH3_TC),a
	call jCTC2_INIT
	call jCTC3_INIT

	ret

	ld a,01110111b      ; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
                         ; start upon loading time constant, time constant follows,sw reset, command word
	out (CTC_CH2),a
	ld A,(CTC_CH2_TC)           ; time constant 
	out (CTC_CH2),a         ; loaded into channel 3 which drives SIOA
	ret


endprog	equ $

	output_bin "sio_no_int_test.bin",main,endprog-main    ; 
	output_intel "sio_no_int_test.hex",main,endprog-main    ;
	output_list "sio_no_int_test.lst"
