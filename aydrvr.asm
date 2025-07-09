ayinit:		push hl
	push bc
	push af
	ld hl,aytab+15
	ld b,$0f
ayini1:	ld a,b
	call aywr
	dec hl
	dec b
	jp p,ayini1
	pop af
	pop bc
	pop hl
	ret

; a contains register address, hl contains address where data to be written is
aywr:	out (aybase),a
	ld a,(hl)
	out (aybase+1),a
	ret

; c contains value to be written, a containd register number
aysetreg: out (aybase),a
	ld a,c
	out (aybase+1),a
	ret

aytab:	defb 0,0,0,0,0,0,0,$f8,$f,$f,$f,0,0,0,0,0

ayblink:	push af
	push bc
	ld c,$0
ayblnk:	ld a,$0e
	call aysetreg
	ld a,$a0
	call delay		; looks like Z80 needs this delay to successfully write to IO ports
;	ld a,$4e				; reset wdt
;	out ($F1),a
	inc c
	jr nz,ayblnk
	ld a,$0e
	call aysetreg
	pop bc
	pop af
	ret