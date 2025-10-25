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

	ld a,11001111b      ; mode 3 (bit control)
	out (PIO_CB),a
	ld a,$0			; set pins of port A to OUTPUT
	out (PIO_CB),a
	out (PIO_DB),a		; zero the output
	ld a,PIOV+2		; set IM2 PIO vector
	out (PIO_CB),a
	ld a,$07			; disable interrupt
	out (PIO_CB),a

	ld a,00000001b      ; write into WR0: select WR1
	out (SIO_CA),a
	ld a,00000100b      ; write into WR1: status affects interrupt vectors
	out (SIO_CA),a
	ld a,00000001b      ; write into WR0: select WR1
	out (SIO_CB),a
	ld a,00000100b      ; write into WR1: status affects interrupt vectors
	out (SIO_CB),a


	ld a,$0
	out ($f4),a		; set INT priority order section 3.9, page 149

	ld a,$24	;high IRQTAB	; load high byte of interrupt vector table address
	ld i,a			; set interrupt vector for IM2
	im 2				; enable interrupt mode 2

	ld hl,$5000
	ld de,$5001
	ld bc,$ff
	ld a,0
	ld (hl),a
	ldir

	ld de,int_tgt
	ld hl,$2400	;IRQTAB
	ld bc,15
	ld a,0

intvinitl:	ld (hl),e		; initialize interrupt jump table
	inc hl
	ld (hl),d
	inc hl
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,intvinitl

	xor a				; make sure A is 0
	ret

do_test:	ld (SP_INIT-$100),sp
	ld sp,SP_INIT-$30
	ld hl,SP_INIT-$100+3
	xor a
	ld (hl),a
	dec hl
	ld (hl),a
	ld ix,$5000

	ld a,CTCV			; load CTC interrupt vector
	out (CTC_CH0),a		; set CTC T0 to that vector

	ld a,$27			; get T0 configuration default
	or a,$80			; enable interrupt bit
	and a,$ff			; no time constant follows
	out (CTC_CH3),a
	ld a,$54			; reset time constant
	out (CTC_CH3),a

int_test: 	ei
;	out (PIO_DB),a
;	inc (hl)
	nop	; give interrupt time to occur during nops rather than outside the loop
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
	or a
	jr z,cont
	pop af
	xor a
;	inc (hl)
	ld sp,(SP_INIT-$100)
	ret
cont:	pop af
;	ei
	or a
	jr z,int_test

;	inc (hl)
;	inc (hl)
	out (PIO_DA),a
	di
;	call jCON_PRINTHBYTE
	ld a,"x"
	call jCON_PRT_CHAR
	call jCON_PRT_NL
	call jCON_RX_CHK
	or a
	jr z,int_test
	di
	xor a
	ld sp,(SP_INIT-$100)
	ret

tc0ei:	ld a,(CTC_CH3_CNF)	; get T0 configuration default
	or a,$80			; enable interrupt bit
	and a,$fb			; no time constant follows
	out (CTC_CH3),a

tc0di:	ld a,(CTC_CH3_CNF)	; get T0 configuration default
	and a,$fb			; no time constant follows
	out (CTC_CH3),a

tc1ei:	ld a,(CTC_CH1_CNF)	; get T0 configuration default
	or a,$80			; enable interrupt bit
	and a,$fb			; no time constant follows
	out (CTC_CH1),a

tc1di:	ld a,(CTC_CH1_CNF)	; get T0 configuration default
	and a,$fb			; no time constant follows
	out (CTC_CH1),a

tc2ei:	ld a,(CTC_CH2_CNF)	; get T0 configuration default
	or a,$80			; enable interrupt bit
	and a,$fb			; no time constant follows
	out (CTC_CH2),a

tc2di:	ld a,(CTC_CH2_CNF)	; get T0 configuration default
	and a,$fb			; no time constant follows
	out (CTC_CH2),a

tc3ei:	ld a,(CTC_CH3_CNF)	; get T0 configuration default
	or a,$80			; enable interrupt bit
	and a,$fb			; no time constant follows
	out (CTC_CH3),a

tc3di:	ld a,(CTC_CH3_CNF)	; get T0 configuration default
	and a,$fb			; no time constant follows
	out (CTC_CH3),a


im1:	im 1
	ld hl,CTC_CH0_CNF
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
	and a,$ff			; $fb -> no time constant follows
	out (CTC_CH0),a
	ld a,(CTC_CH0_TC)	; reset time constant
	out (CTC_CH0),a
	xor a
	ret

doei:	ei
	ret

intsrvc:	di
	out (beepr),a
	in a,(PIO_DB)
	xor a,$ff
	out (PIO_DB),a
;	ld a,(CTC_CH0_CNF)	; get T0 configuration default
;	or a,$80			; enable interrupt bit
;;	and a,!$04			; no time constant follows
;	out (CTC_CH3),a
;	ld a,(CTC_CH3_TC)	; reset time constant
;	out (CTC_CH3),a
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
	
int_tgt:
value=0
	repeat
	di
	in a,(PIO_DB)
	xor a,$ff
	out (PIO_DB),a
	ld a,value+1
; 	ld (SP_INIT-$100+3),a
	inc (ix + value)
	ei
	reti

	value=value+1
	until value=128

value=0
	repeat
	di
	in a,(PIO_DB)
	xor a,$ff
	out (PIO_DB),a
	ld a,value+1-128
; 	ld (SP_INIT-$100+3),a
	inc (ix + value-128)
	ei
	reti

	value=value+1
	until value=128

endprog	equ $

;	output_bin "interrupt_vectors_test.bin",main,endprog-main    ; 
	output_intel "interrupt_vectors_test.hex",main,endprog-main    ;
	output_list "interrupt_vectors_test.lst"
