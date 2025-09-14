ymzinit:	push hl
	push bc
	push af
	ld hl,ymztab+13
	ld b,$0d
ymz1:	ld a,b
	call ymzwr
	dec hl
	dec b
	jp p,ymz1
	pop af
	pop bc
	pop hl
	ret

; a contains register address, hl contains address where data to be written is
ymzwr:	out (ymbase),a
	ld a,(hl)
	out (ymbase+1),a
	ret

; c contains value to be written, a containd register number
ymzsetreg: out (ymbase),a
	ld a,c
	out (ymbase+1),a
	ret

ymztab:	defb $40,0,$20,0,$10,0,0,$f8,$f,$f,$f,0,0,0
