;program the PIO
PIO_INIT:	ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CB),a
        ld a,$ff      ; set pins of port A to INPUT
        out (PIO_CB),a

        ld a,11001111b      ; mode 3 (bit control)
        out (PIO_CA),a
        ld a,$FF      ; set pins of port A to INPUT
        out (PIO_CA),a

        ret

