CTC_INIT_ALL: push af
		call CTC_TC_INIT
		call CTC0_INIT
		call CTC1_INIT
		call CTC2_INIT
		call CTC3_INIT
		pop af
		ret

;CH0 divides CPU CLK by (256*CTC_CH0_TC) providing a clock signal at TO0. TO0 is connected to TRG1.
; T01 outputs f= CPU_CLK/(256*CTC_CH0_TC) => 9.216MHz / ( 256 * 180 ) => 200Hz
CTC0_INIT: ld a,CTCV		; load CTC interrupt vector
	out (CTC_CH0),a		; set CTC T0 to that vector
	ld a,(CTC_CH0_CNF)	; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
					; time constant follows; cont. operation; command word
	out (CTC_CH0),a
	ld a,(CTC_CH0_TC)	; time constant
	out (CTC_CH0),a

;init CH1
;CH1 disabled
CTC1_INIT: ld a,CTCV+2	; load CTC interrupt vector
	out (CTC_CH1),a		; set CTC T0 to that vector
	ld a,(CTC_CH1_CNF)	; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
					; start timer on loading constant, time constant follows, software reset, command word
	out (CTC_CH1),a		; CH1 is halted
	ld A,(CTC_CH1_TC)	; time constant 56d
	out (CTC_CH1),a		; loaded into channel 1

;init CH2
;CH2 divides CLK/TRG2 clock providing a clock signal at TO2.
; T02 outputs f= CLK/TRG / 0x34 => 4MHz / 52 => 38461
CTC2_INIT: ld a,CTCV+4	; load CTC interrupt vector
	out (CTC_CH2),a		; set CTC T0 to that vector
	ld a,(CTC_CH2_CNF)	; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
					; start upon loading time constant, time constant follows,sw reset, command word
	out (CTC_CH2),a
	ld A,(CTC_CH2_TC)	; time constant 56d
	out (CTC_CH2),a		; loaded into channel 2

;init CH3
;CH3 divides CLK/TRG3 clock providing a clock signal at TO3.
; T03 outputs f= CLK/TRG / 4  => 4MHz / 4 => 1MHz
CTC3_INIT: ld a,CTCV+6	; load CTC interrupt vector
	out (CTC_CH3),a		; set CTC T0 to that vector
	ld a,(CTC_CH3_CNF)	; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
					; start upon loading time constant, time constant follows,sw reset, command word
	out (CTC_CH3),a
	ld A,(CTC_CH3_TC)	; time constant 56d
	out (CTC_CH3),a		; loaded into channel 3
	ret


CTC_TC_INIT: ld hl,SYSTMR0
		ld b,10
		xor a
CTC_TC_INIT1:	ld (hl),a
		inc hl
		djnz CTC_TC_INIT1

		ld hl,CTC_DEFAULTS
		ld de,CTC_CH0_CNF
		ld bc,8
		ldir
		ret

; these are configured in CONSTANTS.asm
CTC_DEFAULTS:	db CTC_CH0_CNFV,CTC_CH1_CNFV,CTC_CH2_CNFV,CTC_CH3_CNFV
			db CTC_CH0_TV,CTC_CH1_TV,CTC_CH2_TV,CTC_CH3_TV

; T0 ISR - increment system timer by 1 on every interrupt
; 118 cycles when only lower counter incremented
; 151 cycles when overflow into upper counter = 16.3845486us

CTC_T0_ISR:	push hl	; 11c
		push af		; 11c
		ld hl,(SYSTMR0)	; 16c
		inc hl		; 6c
		ld (SYSTMR0),hl	; 16c
		ld a,h		; 4c
		or l			; 4c
		jr nz,CTC_T0_X	; 12/7c
		ld hl,(SYSTMR2)	; 16c
		inc hl		; 6c
		ld (SYSTMR2),hl	; 16c
CTC_T0_X:	pop af		; 10c
		pop hl		; 10c
		ei			; 4c
		reti			; 14c

; T1 ISR - increment system timer by 1 on every interrupt
; same as T0 but interrupt timing my differ

CTC_T1_ISR:
up_isr:	push af
		ld a,(SYSTMR4)	; 5 milliseconds
		inc a
		cp 200
		jr nz,up_isr_1
		xor a
up_isr_1:	ld (SYSTMR4),a
		jr nz,up_isr_x

		ld a,(SYSTMR5)	; seconds
		inc a
		cp 60
		jr nz,up_isr_2
		xor a
up_isr_2:	ld (SYSTMR5),a
		jr nz,up_isr_x

		ld a,(SYSTMR6)	; minutes
		inc a
		cp 60
		jr nz,up_isr_3
		xor a
up_isr_3:	ld (SYSTMR6),a
		jr nz,up_isr_x

		ld a,(SYSTMR7)	; hours
		inc a
		cp 24
		jr nz,up_isr_4
		xor a
up_isr_4:	ld (SYSTMR7),a
		jr nz,up_isr_x

		push hl
		ld hl,(SYSTMR8)	; days
		inc hl
		ld (SYSTMR8),hl
		pop hl

up_isr_x:	pop af
		ei
		reti

