;initialize both PIO ports
PIO_INIT:	ld a,PIO_CH0_CNFV      ; PIO channel A default config mode 3 (bit control)
	ld (PIO_CH0_CNF),a
	ld a,PIO_CH1_CNFV      ; PIO channel B default config mode 3 (bit control)
	ld (PIO_CH1_CNF),a
	call PIOA_INIT	; first initialize PIO A, then fall through to PIO B

PIOB_INIT:	ld a,(PIO_CH1_CNF)
		out (PIO_CB),a
		ld a,$ff      ; set pins of port A to INPUT
		out (PIO_CB),a
		ret

PIOA_INIT:	ld a,(PIO_CH0_CNF)
        out (PIO_CA),a
        ld a,$FF      ; set pins of port A to INPUT
        out (PIO_CA),a

        ret

PIOA_OUT:	out (PIO_DA),A
	ret

PIOB_OUT:	out (PIO_DB),A
	ret
