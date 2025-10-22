		include "jump.inc"

;-------------------------------------------------------------------------------
; MAIN
;-------------------------------------------------------------------------------

        org $3000

main:	ld a,11001111b      ; mode 3 (bit control)
	out (PIO_CA),a
	ld a,$0			; set pins of port A to OUTPUT
	out (PIO_CA),a
	out (PIO_DA),a		; zero the output
	ld a,PIOV			; set IM2 PIO vector
	out (PIO_CA),a
	ld a,$07			; disable interrupt
	out (PIO_CA),a
	ld a,PIOV			; set IM2 PIO vector
	out (PIO_CB),a
	ld a,$07			; disable interrupt
	out (PIO_CB),a

	ld a,00000001b      ; write into WR0: select WR1
	out (SIO_CA),a
	ld a,00000100b      ; write into WR0: status affects interrupt vectors
	out (SIO_CA),a
	ld a,00000001b      ; write into WR0: select WR1
	out (SIO_CB),a
	ld a,00000100b      ; write into WR0: status affects interrupt vectors
	out (SIO_CB),a


	ld a,high IRQTAB	; load high byte of interrupt vector table address
	ld i,a			; set interrupt vector for IM2
	im 2				; enable interrupt mode 2

	ld a,CTCV			; load CTC interrupt vector
	out (CTC_CH0),a		; set CTC T0 to that vector

	ld de,int_tgt
	ld hl,IRQTAB
	ld bc,$08
	ld a,32

intvinitl:	ld (hl),e		; initialize interrupt jump table
	inc hl
	ld (hl),d
	inc hl
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,intvinitl

;	ld a,(CTC_CH0_CNF)	; get T0 configuration default
;	or a,$80			; enable interrupt bit
;;	and a,!$04			; no time constant follows
;	out (CTC_CH0),a
;	ld a,(CTC_CH0_TC)	; reset time constant
;	out (CTC_CH0),a
	xor a				; make sure A is 0
	ret

do_test:	xor a
	ld sp,$23d0
	ei

int_test: nop	; give interrupt time to occur during nops rather than outside the loop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	di
	push af
	call jCON_RX_CHK
	jr z,cont
	call jCON_RX
	pop af
	xor a
	ret
cont:	pop af
	ei
	or a
	jr z,int_test

	out (PIO_DA),a
	call jCON_PRINTHBYTE
	ld a,"x"
	call jCON_PRT_CHAR
	call jCON_PRT_NL
	call jCON_RX_CHK
	or a
	jr z,int_test
	di
	call jCON_RX
	xor a
	ret

im1:	ld hl,CTC_CH0_CNF
	ld a,$27
	ld (hl),a
	inc hl
	ld a,$07
	ld (hl),a
	inc hl
	ld a,$77
	ld (hl),a
	inc hl
	ld a,$77
	ld (hl),a
	inc hl
	ld a,$46
	ld (hl),a
	ld hl,intsrvc		; get address of isr
	ld ($211d),hl		; insert it into jump instruction of RST38 INT ISR code
	ld a,(CTC_CH0_CNF)	; get T0 configuration default
	or a,$80			; enable interrupt bit
;	and a,!$04			; no time constant follows
	out (CTC_CH0),a
	ld a,(CTC_CH0_TC)	; reset time constant
	out (CTC_CH0),a
	im 1
	xor a
	ret

doei:	ei
	ret

intsrvc:	out (beepr),a
;	ld a,(CTC_CH0_CNF)	; get T0 configuration default
;	or a,$80			; enable interrupt bit
;;	and a,!$04			; no time constant follows
;	out (CTC_CH0),a
;	ld a,(CTC_CH0_TC)	; reset time constant
;	out (CTC_CH0),a
	exx
	ex af,af'
	ei
	reti


buzz: in a,($10)
	ld b,a
bl1:	in a,($10)
	cp b
	jr nc,bl2
	out (beepr),a
bl2:	ld b,a
	jr bl1
	
int_tgt: ld a,1
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,2
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,3
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,4
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,5
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,6
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,7
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,8
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,9
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$a
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$b
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$c
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$d
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$e
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$f
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$10
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$11
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$12
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$13
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$14
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$15
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$16
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$17
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$18
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$19
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1a
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1b
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1c
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1d
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1e
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$1f
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$20
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$21
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$22
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$23
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$24
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$25
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$26
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$27
	call jCON_PRINTHBYTE
	ei
	reti

	ld a,$28
	call jCON_PRINTHBYTE
	ei
	reti

endprog	equ $

;	output_bin "interrupt_vectors_test.bin",main,endprog-main    ; 
	output_intel "interrupt_vectors_test.hex",main,endprog-main    ;
	output_list "interrupt_vectors_test.lst"
