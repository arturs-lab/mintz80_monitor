; demonstrates hooking into CTC ISR to perform additional tasks on CTC interrupt

	org $3000

; install ISR hooks for CTC chnnels 0 and 1
start:	di
	push af
	push hl

	ld a,00000111b		; disable interrupt
	out (PIO_CA),a
	ld a,00001111b		; PIO mode 0 - byte output
	out (PIO_CA),a
	ld a,$0
	out (PIO_DA),a		; zero the output

	ld hl,(IRQTAB+CTCV)	; get originl CTC T0 ISR address
	ld (ctc0_h_j+1),hl	; add it as return after this ISR

	ld hl,(IRQTAB+CTCV+2)	; get originl CTC T1 ISR address
	ld (ctc1_h_j+1),hl	; add it as return after this ISR

	ld hl,ctc0_h		; add hook to ISR by placing my new ISR address where old one was
	ld (IRQTAB+CTCV),hl

	ld hl,ctc1_h		; add hook to ISR by placing my new ISR address where old one was
	ld (IRQTAB+CTCV+2),hl

	pop hl
	pop af
	ld a,0
	ret

; we're not enabling interrupts above to allow user to verify changes which were made before using the hook
do_ei:	ei
	ret

; uninstall ISR hooks
uninstall:	di
		push hl

		ld hl,(ctc0_h_j+1)	; get original CTC0 ISR address
		ld (IRQTAB+CTCV),hl	; stick it into table
		ld hl,(ctc1_h_j+1)	; same for CTC1
		ld (IRQTAB+CTCV+2),hl

		pop hl
		ei
		ret

; hook for CTC0 ISR
ctc0_h:	push af
	in a,(PIO_DA)	; invert bit 0 of PIOA on every CTC0 interrupt
	xor $01
	out (PIO_DA),a
	pop af
ctc0_h_j:	jp 0000		; jump to original ISR. This will be modified by code above which instals this hook

; hook for CTC1 ISR
ctc1_h:	push af
	in a,(PIO_DA)	; invert bit 1 of PIOA on every CTC1 interrupt
	xor $02
	out (PIO_DA),a
	pop af
ctc1_h_j:	jp 0000		; jump to original ISR. This will be modified by code above which instals this hook

endprog	equ $

		include "jump.inc"

;	output_bin "ctc_int_hook.bin",start,endprog-start
	output_intel "ctc_int_hook.hex",start,endprog-start
	output_list "ctc_int_hook.lst"

