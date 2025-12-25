;initialize both PIO ports
PIO_INIT:	push hl
		push de
		push bc
		ld hl,PIO_DEFLT
		ld de,PIOA_CNF
		ld bc,PIO_DEFLT_END-PIO_DEFLT

		ldir

		pop bc
		pop de
		pop hl

		call PIOA_INIT	; first initialize PIO A, then fall through to PIO B

PIOB_INIT:	ld a,(PIOB_CNF)
		out (PIO_CB),a
		ld a,$ff      ; set pins of port B to INPUT
		out (PIO_CB),a
		ret

PIOA_INIT:	ld a,(PIOA_CNF)
		out (PIO_CA),a
		ld a,$FF      ; set pins of port A to INPUT
		out (PIO_CA),a

		ret

PIO_DEFLT:	db PIOA_CNFV,PIOA_INT_CTRV,PIOA_INT_ENV,PIOB_CNFV,PIOB_INT_CTRV,PIOB_INT_ENV

PIO_DEFLT_END:	EQU $

PIOA_OUT:	out (PIO_DA),A
	ret

PIOB_OUT:	out (PIO_DB),A
	ret
