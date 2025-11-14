; Example of installing a new ISR hook

		include "jump.inc"

ROM_BOTTOM_a000:	equ $a000		; this results in code being complied to work with A000 version od monitor

		org $9000

install:	di			; install ISR
		xor a
		ld hl,TIMR_0	; zero variables which will be used
		ld b,8
instl1:	ld (hl),a
		inc hl
		djnz instl1

		ld hl,(IRQTAB+CTCV+2)	; get originl CTC T1 ISR address
		ld (up_isr_j+1),hl	; add it as return after this ISR

		ld hl,up_isr	; add hook to ISR
		ld (IRQTAB+CTCV+2),hl
		ei
		ret

uninstall:	di
		ld hl,(up_isr_j+1)
		ld (IRQTAB+CTCV+2),hl
		ei
		ret

up_isr:	push af
		ld a,(TIMR_0)	; 10 milliseconds
		inc a
		cp 200
		jr nz,up_isr_1
		xor a
up_isr_1:	ld (TIMR_0),a
		jr nz,up_isr_x

		ld a,(TIMR_1)	; seconds
		inc a
		cp 60
		jr nz,up_isr_2
		xor a
up_isr_2:	ld (TIMR_1),a
		jr nz,up_isr_x

		ld a,(TIMR_2)	; minutes
		inc a
		cp 60
		jr nz,up_isr_3
		xor a
up_isr_3:	ld (TIMR_2),a
		jr nz,up_isr_x

		ld a,(TIMR_3)	; hours
		inc a
		cp 60
		jr nz,up_isr_4
		xor a
up_isr_4:	ld (TIMR_3),a
		jr nz,up_isr_x

		push hl
		ld hl,(TIMR_4)	; days
		inc hl
		ld (TIMR_4),hl
		pop hl

up_isr_x:	pop af
up_isr_j:	jp 0		; this gets replaced with address of original ISR to be executed after this one

; or we could just end ISR here
;		ei
;		reti

endprog	equ $

TIMR_0:	ds 1
TIMR_1:	ds 1
TIMR_2:	ds 1
TIMR_3:	ds 1
TIMR_4:	ds 2

;	output_bin "uptime.bin",install,endprog-install
	output_intel "uptime.hex",install,endprog-install
	output_list "uptime.lst"

