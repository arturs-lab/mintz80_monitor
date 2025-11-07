; https://skoolkid.github.io/rom/maps/routines.html

; Bajtek 8/89
; do = ;
; notes # through P
; percussion Q - Y
; b - loop
;
; F618 - F9FF - tables
;
; SAVE "name" CODE 60000 5024

CLKDIV	EQU 0
;CHIP		EQU "AY"
CHIP		EQU "YMZ"

if CHIP = "AY"
AYSEL		EQU	$00
AYDTA		EQU	$01
PLAYER	EQU	$2400
FRQDIV	EQU	$20
else
AYSEL		EQU	$b0
AYDTA		EQU	$b1
;AYSEL		EQU	$02
;AYDTA		EQU	$03
PLAYER	EQU	$3800
FRQDIV	EQU	$10
endif

MUSIC1	EQU	PLAYER + $0500
MUSIC2	EQU	PLAYER + $08E8
MUSIC3	EQU	PLAYER + $0cD0
XTRA		EQU	PLAYER + $10B8
XTRAEND	EQU	XTRA + 385

;MUSIC1	EQU	$EA60
;MUSIC2	EQU	$EE48
;MUSIC3	EQU	$F230
;XTRA		EQU	$F618
;XTRAEND	EQU	XTRA + 385
;PLAYER	EQU	$FA00

	ORG	PLAYER

FA00	ld a,$10	; frequency source
	out (04),a
	ld a,FRQDIV	; freq divider
	out (05),a
	in a,($d0)		; get current clock divider
	push af
if CLKDIV = 0
	ld a,$0	; cpu frequency
else
	ld a,$1	; cpu frequency
endif
	out ($d0),a
if CLKDIV = 0
	ld a,$c0		; playback speed
else
	ld a,$e0		; playback speed
endif
	ld (PBSPEED),a
	call jCON_PRT_NL
	jp	FBA5		; initialize note pointers and start playing


; updated by $FBF4, $FCAB
; 
FA0F	DB	$0F		; channel 1

; updated by $FBF4
; 
FA10	DB	$0F,$0F	; channel 2 and 3

; AY sound register values
RTPERIOD EQU $
FA12	DB	$A8,$01	; channel 1 period
FA14	DB	$9F,$06	; channel 2 period
FA16	DB	$3D,$01	; channel 3 period

; FA18
RNOISEP	DB	$1E	; noise period updated by FC39

; FA19
RENABLE	DB	$FB	; enable bits updated by $FB51 $FBF4 $FC48

; FA1A
RAMPLIT	DB	$00,$00,$0F	; amplitudes updated by $FC63 $FDD5 

; FA1D
RENVPER	DB	$00,$00	; envelope period updated by none

; FA1F
RENVSHAP	DB	$00	; envelope shape updated by none

; updated by $FB51 $FC72
; read by $FDD5
; FA20
FA20	DB	$10

; RAMPLIT + 7
; updated by $FC39 $FBF4 $FC48 $FDD5
FA21	DB	$00,$00,$00

; updated by $FBF4 $FCF5 $FD0F
FA24	DB	$00,$00,$00 

; updated by $FB51 $FCDF
; read by $FD0F
FA27	DB	$00

; updated by $FC72 
; read by $FD95
FA28	DB	$00,$00,$00,$00,$00,$00

; updated by $FB51 $FBA5 $FD0F
; read by $FD43
; FA2E
CHAMUSICPTR	DW	MUSIC1	; channel A music data address, $EA60
	DW	MUSIC1+1
; FA32
CHBMUSICPTR	DW	MUSIC2	; channel B music data address, $EE48
	DW	MUSIC2+1
; FA36
CHCMUSICPTR	DW	MUSIC3	; channel C music data address, $F230
	DW	MUSIC3+1

; updated by $FB51 $FBA5
EFFECTPTR	EQU $
FA3A	DB	XTRA,XTRA>>8	; some other table, 1000 bytes up from $F23C, CHCMUSICPTR, should be $F618 per new locations
	DW	XTRA+1

; read by $FC72
PBSPEED	DB	$f3	;$E6	; playback speed @CPU CLK: 2MHz->$f3, 4MHz->$e2

; read by $FDD5
FA3F	DB	$08,$07,$06,$05,$06,$07,$08,$09,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B
	DB	$0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01,$00
	DB	$0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
	DB	$04,$06,$08,$0A,$0B,$0C,$0D,$0E,$0F,$0E,$0D,$0C,$0B,$0A,$07,$03
	DB	$08,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0B,$09,$08
	DB	$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$08,$05,$02,$02,$03,$03,$03,$03
	DB	$08,$0B,$0D,$0E,$0F,$0F,$0E,$0D,$0C,$0B,$0A,$09,$09,$09,$09,$09
	DB	$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$00,$0F,$08

; note to reg value mapping?
; read by $FBD0 $FD43
FABF	EQU	$
N_TO_REG	DB	$9F
FAC0	DB	$06,$40,$06,$E6,$05,$91,$05,$41,$05,$F6,$04,$AE,$04,$6B,$04,$2C
FAD0	DB	$04,$F0,$03,$B7,$03,$82,$03,$4F,$03,$20,$03,$F3,$02,$C8,$02,$A1
FAE0	DB	$02,$7B,$02,$57,$02,$36,$02,$16,$02,$F8,$01,$DC,$01,$C1,$01,$A8
FAF0	DB	$01,$90,$01,$79,$01,$64,$01,$50,$01,$3D,$01,$2C,$01,$1B,$01,$0B
FB00	DB	$01,$FC,$00,$EE,$00,$E0,$00,$D4,$00,$C8,$00,$BD,$00,$B2,$00,$A8
FB10	DB	$00,$9F,$00,$96,$00,$8D,$00,$85,$00,$7E,$00,$77,$00,$70,$00,$6A
FB20	DB	$00,$64,$00,$5E,$00,$59,$00,$54,$00
FB29	DB	$FF,$FF,$08,$09,$18,$09,$00
FB30	DB	$80,$1C,$80,$04,$10,$0C,$10,$14,$10,$1C,$80,$1E,$80,$00,$00 

; updated by $FCDF
; read by $FD0F
FB3F	DB	$00,$00,$00,$00,$00,$00 


FB45	call	FB51	; play notes for all channels?
	call	rtrn		; $28E	;poll keyboard
	inc	e
	jr	z,FB45	; continue loop
	jp	FBC7	; unless key pressed - then exit

FB51	di			; make sure interrupts turned off
	ld	hl,RENABLE	; set enable register to all channels disabled + noise off
	ld	b,$FF
	ld	(hl),b
	inc	hl		; point at RAMPLIT
	inc	b		; then set amplitudes to 0
	call	FCD9		; store B -> (HL) 3 times incrementing HL each time
	ld	hl,FA20	; zero 
	ld	(hl),b
	inc	hl
	call	FCD9		; store B -> (HL) 3 times incrementing HL each time
	ld	hl,FA27	; zero
	ld	(hl),b
	inc	hl
	call	FCD9		; store B -> (HL) 3 times incrementing HL each time
	inc	hl
	call	FCD9		; store B -> (HL) 3 times incrementing HL each time
	ld	hl,EFFECTPTR	; get effect
	call	FBD0		; get reg value
	ld a,b
	jr z,prsp
	add a,$f8
	and a,$7f
	jr prs
prsp:	add a," "
prs:	call jCON_TX
	ld a," "
	call jCON_TX
	call	FC9D		; process it
	ld	hl,CHAMUSICPTR	; channel A music data address
	call	FBD0		; get note and reg value
	ld a,"#"
	add b
	call jCON_TX
	ld a," "
	call jCON_TX
	ld	a,$01		; channel number
	call	FBF4
	ld	hl,CHBMUSICPTR	; channel B music data address
	call	FBD0		; get note and reg value
	ld a,"#"
	add b
	call jCON_TX
	ld a," "
	call jCON_TX
	ld	a,$02		; channel number
	call	FBF4
	ld	hl,CHCMUSICPTR	; channel C music data address
	call	FBD0		; get note and reg value
	ld a,"#"
	add b
	call jCON_TX
	ld a," "
	call jCON_TX
	ld	a,$03		; channel number
	call	FBF4
	call jCON_PRT_NL
	call	FD15
	call	AYSETREG
	call	FC72
	ret

; initialize note pointers 
FBA5	ld	hl,MUSIC1	; EA60 location of music
	ld	de,CHAMUSICPTR	; 
	ld	bc,$03E8	; 1000 decimal - length of channel data
	call	FBBD		; store location of data for channel A - EA60 -> FA2E
	call	FBBD		; store location of data for channel B - EE48 -> FA32
	call	FBBD		; store location of data for channel C - F230 -> FA36
	call	FBBD		; store location of data for effect? - F618 -> FA3A
	jp	FB45		; start playing

FBBD	ld	a,l
	ld	(de),a
	inc	de
	ld	a,h
	ld	(de),a
	inc	de
	add	hl,bc
	inc	de
	inc	de
	ret

; exit player
FBC7	ld	d,$07	; Write FF to register 7 - disable all channels. Should be 3F to not turn on output on IO ports
	ld	e,$FF
	call	AYWRITE
;	ei
	pop af			; restore original clock
	out ($d0),a
	xor a				; return with A=0
	ret		; exit

; entry:
; address of current note pointer in HL
; returns current note in B
; returns register value for that note in HL
FBD0	ld	e,(hl)	; HL points to address of current note pointer
	inc	hl	; load that pointer into DE
	ld	d,(hl)
	inc	de	; point to next note
FBD4	ld	a,(de)	; get sample
	cp	$3F	; check if loop command 'b'
	jp	z,FBEB	; jump if loop command
	ld	b,a	; save current note in B
	ld	(hl),d	; save current note pointer
	dec	hl
	ld	(hl),e
	add	a,a	; note * 2
	ld	e,a	; turn it into offset to 16 bit table
	ld	d,$00
	ld	hl,N_TO_REG	; get address of beginning of note to reg value map
	add	hl,de		; add offset
	ld	d,(hl)		; get value from table
	inc	hl
	ld	e,(hl)
	ex	de,hl		; put it in HL
	ret

; restart music
FBEB	inc	hl	; after current sample register we have loop dest address register
	ld	e,(hl)	; get that address
	inc	hl
	ld	d,(hl)
	dec	hl	; then point back to current sample
	dec	hl
	jp	FBD4	; and continue loop

; entry 
; A = channel number
; B = note number
; HL = note register value
;
; updates table $FA10
; updates table $FA21
FBF4	ld	c,a		; save channel number in C for later
	ld	a,b		; get note number
	cp	$36		; is it valid range? $36 or less?
	jr	nc,FC39	; no, it's percussion or noisse?
	ld	a,$FF		; is H = $FF ?
	cp	h
	ret	z		; return if so
	ld	a,c		; get channel number
	push	hl		; save HL for later - contains register value for note
	add	a,a		; 16 bit offset to channel data
	dec	a
	ld	hl,FA12	; point to table, counting from A=1 to 3, so 1,3 or 5
	ld	d,$00
	ld	e,a
	add	hl,de		; add 16 bit offset, results in fa13, fa15 or fa17 - starting with MSB
	pop	de		; get note register value
	ld	(hl),e		; store DE in table
	dec	hl
	ld	(hl),d
	ld	hl,FA24	; point to table
	ld	d,$00
	ld	e,c		; load channel number
	add	hl,de		; add 8 bit offset - 1
	dec	hl
	ld	a,(hl)		; get value
	dec	hl
	dec	hl
	dec	hl
	ld	(hl),a		; store in corresponding address of table at FA21
	ld	hl,FA0F	; point to table
	add	hl,de		; DE = channel number
	dec	hl
	ld	a,(hl)		; get amplitude value
	ld	hl,RENABLE	; select amplitude shadow table
	add	hl,de		; add offset to channel
	ld	(hl),a		; store amplitude value
	ld	hl,RENABLE	; select channel enable shadow reg
	dec	c		; decrement channel number
	jp	nz,FC2E		; skip if not zero
	res	0,(hl)		; if 0 turn on channel A in channel enable shadow reg
FC2E	dec	c		; decrement channel number
	jp	nz,FC34		; skip if not zero
	res	1,(hl)		; if 0 turn on channel B in channel enable shadow reg
FC34	dec	c		; decrement channel number
	ret	nz		; skip if not zero
	res	2,(hl)		; if 0 turn on channel C in channel enable shadow reg
	ret

FC39	ld	a,h
	ld	(RNOISEP),a	; put H in noise period shadow reg
	ld	a,l
	and	$F0
	cp	$00
	jr	z,FC63		; jump if upper nibble of L is 0
	rrca
	rrca
	rrca
	rrca
FC48	ld	hl,FA21	; point to table
	ld	d,$00		; add offset
	ld	e,c
	add	hl,de
	dec	hl
	ld	(hl),a		; store in table
	ld	hl,RENABLE
	dec	c
	jr	nz,FC5A
	res	3,(hl)		; if 0 turn on effect A in channel enable shadow reg
	ret
FC5A	dec	c
	jr	nz,FC60
	res	4,(hl)		; if 0 turn on effect B in channel enable shadow reg
	ret
FC60	res	5,(hl)		; if 0 turn on effect C in channel enable shadow reg
	ret

FC63	ld	a,l		; get lower nibble of L
	and	$0F
	ld	hl,RAMPLIT	; point to amplitude shadow registers
	ld	d,$00
	ld	e,c
	add	hl,de		; add offset to register in question
	dec	hl
	ld	(hl),a		; set amplitude
	xor	a		; zero A
	jr	FC48		; loop back

FC72	ld	a,$01		; channel A
	call	FDD5
	ld	a,$02		; channel B
	call	FDD5
	ld	a,$03		; channel C
	call	FDD5
	call	AYSETREG		; send shadow values to AY regs
	ld	a,(PBSPEED)	; get playback speed
FC87	ld	b,$32		; delay loop
FC89	nop
	djnz	FC89
	inc	a
	jr	nz,FC87
	call	UPDPERVAL	; update 3 channel period values from table at $FA28
	ld	hl,FA20	; increment $FA20 counter
	inc	(hl)
	ld	a,(hl)
	cp	$10		; did it reach $10?
	ret	z		; yes, return
	jp	FC72		; no, continue loop

; effect handler?
FC9D	ld	a,b
	and	$03		; put 2 lowest bits of B in C
	ld	c,a
	bit	2,b		; bit 2 flag
	jr	nz,FCAB
	bit	3,b		; bit 3 flag
	jp	nz,FCDF
	ret

FCAB	bit	3,b
	jp	nz,FCF5
	ld	a,$F0		; bit 2 on bit 3 off
	and	b		; get upprt nibble of B
	rrc	a
	rrc	a
	rrc	a
	rrc	a
	ld	b,a		; put it in B
	xor	a		; zero A
	cp	c		; is C 0?
	jr	z,FCCE		; yes
	ld	hl,FA0F	; no, point to 
	ld	d,$00
	ld	e,c
	add	hl,de		; add offset
	dec	hl
	ld	(hl),b		; put upper nibble of B in table
	ld	b,$00
	jp	FD05
FCCE	ld	hl,FA0F	; C was 0, point to 
	call	FCD9		; Store B in 3 channel shadow regs
	ld	b,$00
	jp	FD0F

FCD9	ld	(hl),b		; store B in 3 channels
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),b
	ret

FCDF	ld	a,c		; bit 2 off, bit 3 on
	ld	(FB3F),a	; put lowest 2 bits of B in FB3F
	ld	a,$F0
	and	b		; get high nibble
	rlca
	rlca
	rlca
	bit	4,b
	ld	b,a
	jr	z,FCF1
	xor	$7F
	inc	a
FCF1	ld	(FA27),a
	ret

FCF5	bit	4,b		; bit 2 on, bit 3 on
	ret	nz
	ld	a,$E0		; get 3 highest bits of B
	and	b
	rlca
	rlc	a
	rlc	a
	ld	b,a		; put 3 highest bits in bottom
	xor	a		; zero A
	cp	c		; is C 0?
	jr	z,FD0F		; yes
FD05	ld	d,$00		; no, use it as offset to FA24 table
	ld	e,c
	ld	hl,FA24
	add	hl,de
	dec	hl
	ld	(hl),b		; put 3 highest bits of B in table
	ret

FD0F	ld	hl,FA24	; point to FA24 table
	jp	FCD9		; sotre B in 3 channels
FD15	ld	hl,FA27	; point to FA27 table
	xor	a
	cp	(hl)		; is it 0 ?
	ret	z		; yes, done
	ld	b,(hl)		; no , load it into B
	ld	a,(FB3F)	; load A from F3BF
	cp	$00
	jr	z,FD43		; skip if 0
	ld	ix,RTPERIOD	; period of channel A
	ld	hl,CHAMUSICPTR	; pointer to channel A music
	dec	a
	jp	z,FD5E		; skip if 0
	ld	hl,CHBMUSICPTR
	inc	ix
	inc	ix		; period of channel B
	dec	a
	jp	z,FD5E		; skip if 0
	ld	hl,CHCMUSICPTR
	inc	ix
	inc	ix		; period of channel C
	jp	FD5E		; jump to FD5E

FD43	ld	hl,CHAMUSICPTR	; pointer to channel A music
	ld	ix,RTPERIOD	; period of channel A
	call	FD5E
	inc	ix
	inc	ix		; period of channel B
	ld	hl,CHBMUSICPTR	; channel B
	call	FD5E
	inc	ix
	inc	ix		; period of channel C
	ld	hl,CHCMUSICPTR	; channel C
FD5E	ld	e,(hl)		; get current note from music
	inc	hl
	ld	d,(hl)
	ld	a,(de)
	add	a,b		; add B
	cp	$35
	ret	nc		; return if A <= $35 (53 dec)
	ld	d,$00
	add	a,a
	ld	e,a		; DE = 2 * A
	ld	hl,N_TO_REG	; point to $FABF table
	add	hl,de
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		; get 16 bit value
	ld	l,(ix+$00)	; get current period for channel
	ld	h,(ix+$01)
	ex	de,hl
	xor	a		; clear C flag
	sbc	hl,de		; subtract value from current period
	ld	a,l		; switch bytes
	ld	l,h
	ld	h,a
	xor	a
	push	bc
	ld	b,$04
FD82	xor	$00		; divide HL by 16 ?
	rr	h
	rr	l
	rr	a
	djnz	FD82
	or	h
	ld	(ix+$16),a
	ld	(ix+$17),l
	pop	bc
	ret

; update period value for 3 channels
FD95	EQU	$
UPDPERVAL	ld	hl,RTPERIOD
	ld	de,FA28
	ld	b,$03		; for 3 channels
FD9D	ld	a,(hl)		; get channel period in DE
	push	hl
	inc	hl
	ld	h,(hl)
	ld	l,a
	ex	de,hl
	ld	a,(hl)		; get entry from $FA28 in HL
	inc	hl
	push	hl
	ld	h,(hl)
	ld	l,a
	add	hl,de		; add DE (channel period) and HL ($FA28)
	jr	nc,FDAC
	inc	hl
FDAC	ld	c,h
	ld	a,l
	pop	de		; pop pointer to $FA28
	inc	de
	pop	hl		; pop pointer to channel period
	ld	(hl),a		; store new value in channel period
	inc	hl
	ld	(hl),c
	inc	hl		; point to next channel
	djnz	FD9D		; loop through channels
	ret

AYWRITE	push	bc		; write value in E to AY register given in D
	ld	bc,AYSEL	; AY register select
	out	(c),d
	ld	bc,AYDTA	; AY register value
	out	(c),e
	pop	bc
	inc	d		; point to next AY register
	ret

FDC6	EQU	$
AYSETREG	ld	hl,RTPERIOD	; set AY registers with values pointed to by HL
	ld	d,$00		; point to first AY register
	ld	b,$0E		; count of registers to init
FDCD	ld	e,(hl)		; load value to be put in register
	inc	hl		; point to next value
	call	AYWRITE		; write to reg
	djnz	FDCD		; loop through count in B
	ret

; entry
; A = channel number
; exit
; channel amplitude updated based on value of $FA20 and lookups in $FA21 and $FA3F tables
;
; $FA3F may be wave shape table
; get value from $FA21 table and put it in channel's RAMPLIT register
FDD5	ld	hl,RAMPLIT	; point to amplitudes table
	dec	a
	ld	e,a
	ld	d,$00
	add	hl,de
	ex	de,hl		; DE = channel amplitude
	ld	hl,$0007	; add 7 to offset - that ends up being channel offset into $FA21 table
	add	hl,de
	ld	a,(hl)		; get value
	cp	$00
	ret	z		; return if 0
	push	de		; save pointer to channel amplitude
	ld	hl,FA3F	; 16 byte tables for each channel
	dec	a		; get channel number
	rla
	rla
	rla
	rla
	ld	d,$00
	ld	e,a
	add	hl,de		; HL = $FA3F + ($FA21 + CH#) * 16
	ld	a,(FA20)	; get offset from FA20
	ld	e,a
	add	hl,de		; add offset in table for given channel
	ld	a,(hl)		; get value
	pop	hl		; get pointer to channel amplitude
	ld	(hl),a		; put value there
	ret

rtrn	ld e,$ff
	call jCON_RX_CHK
	add $ff
	ld e,a
	ret

	ORG	MUSIC1

// beverly hills cop
EA60	DB	$00,$28,$35,$35,$35,$2B,$35,$35,$28,$35,$28,$2D,$35,$28,$35,$26
	DB	$35,$28,$35,$35,$35,$2F,$35,$35,$28,$35,$28,$30,$35,$2F,$35,$2B
	DB	$35,$28,$35,$2F,$35,$28,$35,$28,$26,$35,$26,$23,$35,$2A,$35,$28
	DB	$35,$35,$35,$35,$35,$35,$35,$35,$35,$04,$06,$07,$09,$0B,$0D,$0F
	DB	$10,$04,$35,$35,$35,$10,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$35,$35,$35,$10,$35,$35,$35,$35,$0B,$0B,$35,$0E,$35,$10
	DB	$35,$07,$35,$35,$35,$0C,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$35,$35,$35,$10,$35,$35,$35,$35,$10,$0E,$35,$0B,$35,$07
	DB	$35,$04,$35,$35,$35,$10,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$35,$35,$10,$35,$35,$35,$35,$0B,$0B,$35,$0E,$35,$10
	DB	$35,$07,$37,$35,$35,$0C,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$35,$35,$35,$10,$35,$35,$35,$35,$10,$0E,$35,$0B,$35,$07
	DB	$35,$04,$37,$35,$35,$10,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$35,$35,$10,$35,$35,$35,$35,$0B,$0B,$35,$0E,$35,$10
	DB	$35,$07,$37,$35,$35,$0C,$35,$35,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$35,$35,$35,$10,$35,$35,$35,$35,$10,$0E,$35,$0B,$35,$07
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$35,$35,$0B,$0B,$35,$0E,$35,$10
	DB	$35,$07,$37,$36,$35,$0C,$35,$37,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$35,$35,$10,$0E,$3E,$0B,$35,$07
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$35,$35,$0B,$0B,$35,$0E,$35,$10
	DB	$35,$07,$37,$36,$35,$0C,$35,$37,$0E,$35,$0E,$0B,$35,$0E,$35,$10
	DB	$35,$04,$37,$36,$35,$10,$35,$37,$35,$35,$10,$0E,$3E,$0B,$35,$07
	DB	$35,$3F

	ORG	MUSIC2

EE48	DB	$35,$35,$1C,$35,$35,$1C,$1F,$35,$1F,$1C,$35,$1C,$21,$21,$1C,$1C
	DB	$1A,$1A,$1C,$35,$35,$1C,$23,$35,$23,$1C,$35,$1C,$24,$24,$23,$23
	DB	$1F,$1F,$1C,$1C,$23,$23,$28,$23,$1C,$1A,$1C,$1A,$1A,$17,$17,$1E
	DB	$1C,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$35,$3B,$3A
	DB	$3A,$35,$04,$35,$36,$35,$35,$35,$37,$35,$35,$0E,$35,$0B,$35,$0E
	DB	$35,$10,$04,$35,$36,$35,$35,$35,$37,$35,$35,$35,$35,$0B,$35,$0E
	DB	$35,$10,$07,$35,$36,$35,$35,$35,$37,$35,$35,$0E,$35,$0B,$35,$0E
	DB	$35,$10,$04,$35,$36,$35,$35,$35,$37,$35,$3E,$35,$0E,$3E,$0B,$3E
	DB	$07,$39,$1C,$35,$36,$3E,$1F,$35,$37,$1C,$38,$38,$21,$3E,$1C,$3E
	DB	$1A,$39,$1C,$35,$36,$3E,$23,$35,$37,$1C,$38,$38,$24,$3E,$23,$3E
	DB	$1F,$39,$1C,$36,$23,$3E,$28,$37,$37,$1A,$38,$38,$17,$3E,$1E,$3E
	DB	$1C,$39,$37,$35,$36,$3E,$35,$35,$37,$35,$35,$3E,$0E,$3E,$0B,$3E
	DB	$35,$39,$1C,$35,$36,$3E,$1F,$35,$37,$1C,$38,$38,$21,$3E,$1C,$3E
	DB	$1A,$39,$1C,$35,$36,$3E,$23,$35,$37,$1C,$38,$38,$24,$3E,$23,$3E
	DB	$1F,$39,$1C,$36,$23,$3E,$28,$37,$37,$1A,$38,$38,$17,$3E,$1E,$3E
	DB	$1C,$39,$37,$35,$36,$3E,$35,$35,$37,$35,$35,$3E,$0E,$3E,$0B,$3E
	DB	$07,$39,$04,$23,$28,$3E,$28,$23,$37,$2B,$38,$38,$0B,$3E,$2A,$3E
	DB	$2A,$39,$35,$23,$28,$3E,$28,$23,$37,$2B,$38,$38,$2A,$3E,$28,$3E
	DB	$35,$39,$07,$1F,$24,$3E,$24,$1F,$37,$0E,$38,$38,$0B,$3E,$35,$3E
	DB	$10,$39,$26,$23,$28,$3E,$28,$23,$37,$23,$38,$3E,$0E,$3E,$0B,$3E
	DB	$07,$39,$35,$23,$28,$3E,$28,$23,$37,$2B,$38,$38,$0B,$3E,$2A,$3E
	DB	$2A,$39,$35,$23,$28,$3E,$28,$23,$37,$2B,$38,$38,$2A,$3E,$28,$3E
	DB	$35,$39,$35,$1F,$24,$3E,$24,$1F,$37,$0E,$38,$38,$0B,$3E,$35,$3E
	DB	$26,$39,$26,$23,$28,$3E,$28,$23,$37,$23,$38,$3E,$0E,$3E,$0B,$3E
	DB	$07,$3F

	ORG	MUSIC3

F230	DB	$1C,$1C,$35,$1C,$1C,$1F,$1C,$1F,$1C,$1F,$1C,$21,$1C,$1C,$21,$1A
	DB	$1C,$1C,$1C,$1C,$1C,$23,$1C,$23,$1C,$23,$1C,$24,$1C,$23,$24,$1F
	DB	$23,$1C,$1F,$23,$1C,$28,$23,$1C,$1A,$1C,$1A,$17,$17,$1E,$1E,$1C
	DB	$1E,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$3D,$3D,$3C,$3C,$3B,$35,$35
	DB	$35,$39,$37,$36,$35,$35,$10,$37,$35,$0E,$35,$35,$0B,$35,$0E,$35
	DB	$10,$39,$37,$36,$35,$35,$10,$37,$35,$35,$35,$35,$0B,$35,$0E,$35
	DB	$10,$39,$37,$36,$35,$35,$0C,$37,$35,$0E,$35,$35,$0B,$35,$0E,$35
	DB	$10,$39,$37,$36,$35,$35,$10,$37,$35,$35,$35,$10,$3E,$0E,$3E,$0B
	DB	$3E,$1C,$37,$36,$35,$1F,$10,$37,$1C,$0E,$1C,$21,$0B,$1C,$0E,$1A
	DB	$10,$1C,$04,$36,$35,$23,$10,$37,$1C,$35,$1C,$24,$0B,$23,$0E,$1F
	DB	$10,$1C,$07,$23,$36,$28,$0C,$1C,$1A,$0E,$1A,$17,$0B,$1E,$0E,$1C
	DB	$10,$35,$04,$36,$35,$35,$10,$37,$35,$35,$3E,$10,$3E,$0E,$3E,$0B
	DB	$3E,$1C,$04,$36,$35,$1F,$10,$37,$1C,$0E,$1C,$21,$0B,$1C,$0E,$1A
	DB	$10,$1C,$04,$36,$35,$23,$10,$37,$1C,$35,$1C,$24,$0B,$23,$0E,$1F
	DB	$10,$1C,$07,$23,$36,$28,$0C,$1C,$1A,$0E,$1A,$17,$0B,$1E,$0E,$1C
	DB	$10,$35,$04,$36,$35,$35,$10,$37,$35,$35,$3E,$10,$3E,$0E,$3E,$0B
	DB	$3E,$07,$35,$28,$36,$28,$10,$28,$2B,$0E,$2B,$35,$2B,$2A,$0E,$2A
	DB	$10,$35,$04,$28,$36,$28,$10,$28,$2B,$35,$2B,$2A,$0B,$28,$0E,$35
	DB	$10,$35,$35,$24,$36,$24,$0C,$24,$35,$24,$26,$35,$26,$35,$0E,$26
	DB	$35,$26,$04,$28,$36,$28,$10,$28,$35,$26,$28,$35,$28,$35,$3E,$35
	DB	$3E,$35,$04,$28,$36,$28,$10,$28,$2B,$0E,$2B,$35,$2B,$2A,$0E,$2A
	DB	$10,$35,$04,$28,$36,$28,$10,$28,$2B,$35,$2B,$2A,$0B,$28,$0E,$35
	DB	$10,$35,$07,$24,$36,$24,$0C,$24,$35,$24,$26,$35,$26,$35,$0E,$26
	DB	$10,$26,$04,$28,$36,$28,$10,$28,$35,$26,$28,$10,$28,$35,$3E,$35
	DB	$3E,$3F


MUSICEND	EQU $

; EA60	DB	"#KXXXNXXKXKPXKXIXKXXXRXXKXKSXRXNXKXRXKXKIXIFXMXKXXXXXXXXX')*,.023'XXX3XX1X1.X1X3X'XXX3XXXX..X1X3X*XXX/XX1X1.X1X3X'XXX3XXXX31X.X*X'XXX3XX1X1.X1X3X'ZXX3XXXX..X1X3X*ZXX/XX1X1.X1X3X'XXX3XXXX31X.X*X'ZXX3XX1X1.X1X3X'ZXX3XXXX..X1X3X*ZXX/XX1X1.X1X3X'XXX3XXXX31X.X*X'ZYX3XZ1X1.X1X3X'ZYX3XZXX..X1X3X*ZYX/XZ1X1.X1X3X'ZYX3XZXX31a.X*X'ZYX3XZ1X1.X1X3X'ZYX3XZXX..X1X3X*ZYX/XZ1X1.X1X3X'ZYX3XZXX31a.X*Xb"

; EE48	DB	"XX?XX?BXB?X?DD??==?XX?FXF?X?GGFFBB??FFKF?=?==::A?XXXXXXXXXXXXX^]]X'XYXXXZXX1X.X1X3'XYXXXZXXXX.X1X3*XYXXXZXX1X.X1X3'XYXXXZXaX1a.a*\?XYaBXZ?[[Da?a=\?XYaFXZ?[[GaFaB\?YFaKZZ=[[:aAa?\ZXYaXXZXXa1a.aX\?XYaBXZ?[[Da?a=\?XYaFXZ?[[GaFaB\?YFaKZZ=[[:aAa?\ZXYaXXZXXa1a.a*\'FKaKFZN[[.aMaM\XFKaKFZN[[MaKaX\*BGaGBZ1[[.aXa3\IFKaKFZF[a1a.a*\XFKaKFZN[[.aMaM\XFKaKFZN[[MaKaX\XBGaGBZ1[[.aXaI\IFKaKFZF[a1a.a*b"

; F230	DB	"??X??B?B?B?D??D=?????F?F?F?G?FGBF?BF?KF?=?=::AA?A????????``__^XXX\ZYXX3ZX1XX.X1X3\ZYXX3ZXXXX.X1X3\ZYXX/ZX1XX.X1X3\ZYXX3ZXXX3a1a.a?ZYXB3Z?1?D.?1=3?'YXF3Z?X?G.F1B3?*FYK/?=1=:.A1?3X'YXX3ZXXa3a1a.a?'YXB3Z?1?D.?1=3?'YXF3Z?X?G.F1B3?*FYK/?=1=:.A1?3X'YXX3ZXXa3a1a.a*XKYK3KN1NXNM1M3X'KYK3KNXNM.K1X3XXGYG/GXGIXIX1IXI'KYK3KXIKXKXaXaX'KYK3KN1NXNM1M3X'KYK3KNXNM.K1X3X*GYG/GXGIXIX1I3I'KYK3KXIK3KXaXab"


	ORG	XTRA


; this was a section in Bajtek article that pokes values into addresses starting at 0
; likely a bug in code. Should have been i+ADDR, not just i
; for i=0..poke
	DB	$6D,$EF,$CE,$57,$00,$EF,$57,$00,$EF,$57,$EF,$00,$57,$EF,$57,$EF,$57,$EF,$57,$00
	DB	$00,$EF,$57,$00,$EF,$57,$EF,$00,$57,$EF,$57,$EF,$57,$EF,$57,$EF
	DB	$57,$EF,$57,$EF,$00,$57,$EF,$00,$57,$EF,$57,$EF,$57,$44,$00,$00
	DB	$00,$34,$00,$00,$75,$49,$29,$49,$49,$49,$49,$29,$EC

	ORG	XTRAEND

;F799	DB	$3f
XTRAEND	DB	$3f

		include "jump.inc"

endprog	equ $

if CHIP = "AY"
	output_bin "aytest.bin",PLAYER,endprog-PLAYER		; The binary file
	output_intel "aytest.hex",PLAYER,endprog-PLAYER		; The binary file
	output_list "aytest.lst"
else
	output_bin "ymztest.bin",PLAYER,endprog-PLAYER		; The binary file
	output_intel "ymztest.hex",PLAYER,endprog-PLAYER		; The binary file
	output_list "ymztest.lst"
endif
