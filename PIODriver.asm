;initialize both PIO ports
PIO_INIT: call PIOA_INIT	; first initialize PIO A, then fall through to PIO B

PIOB_INIT:	ld a,11001111b      ; mode 3 (bit control)
		out (PIO_CB),a
		ld a,$ff      ; set pins of port A to INPUT
		out (PIO_CB),a
		ret

PIOA_INIT:	ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CA),a
        ld a,$FF      ; set pins of port A to INPUT
        out (PIO_CA),a

        ret

PIOA_OUT:	out (PIO_DA),A
	ret

PIOB_OUT:	out (PIO_DB),A
	ret
