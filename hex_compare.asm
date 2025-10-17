; compare two ranges of memory
; show chunks where memory differs

	include "jump.inc"

	org $3000

hex_compare:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Compare ranges of memory",$0d,$0a,"First range start ",EOS
zoWarnFlow = true
	call jCON_GETHEXWORD
	LD A, (ERRFLAG)
	CP E_NONE
	RET NZ
	LD (MVADDR+0), HL

	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Second range start ",EOS
zoWarnFlow = true
	call jCON_GETHEXWORD
	LD A, (ERRFLAG)
	CP E_NONE
	RET NZ
	LD (MVADDR+2), HL

	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Length ",EOS
zoWarnFlow = true
	call jCON_GETHEXWORD
	LD A, (ERRFLAG)
	CP E_NONE
	RET NZ
	ld bc,(MVADDR+0)
	add hl,bc			; calculate ending address, will be easier to compare
	ld a,l			; make it an increment of 16 bytes
	and a,$0f
	jr z,cp_lp5
	inc h
	ld a,l
	and a,$f0
	ld l,a
cp_lp5:	LD (MVADDR+4), HL

	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F", 0Dh, 0Ah, EOS
zoWarnFlow = true

	ld hl,(MVADDR+0)
	ld de,(MVADDR+2)
cp_lp3:	ld bc,16
cp_lp1:	ld a,(de)	; get byte to compare
	cp a,(hl)		; compare with corresponding byte from other range
	jr nz,cp_lp2	; if not equal, dump hex both 16 byte chunks
	inc hl		; otherwise go to next byte
	inc de
	dec bc		; see if entire 16 byte chunk processed
	ld a,b
	or a,c
	jr nz,cp_lp1	; no, keep checking
cp_lp4:	ld (MVADDR+0),hl	; store starting point of next chunk
	ld (MVADDR+2),de
	ld bc,(MVADDR+4)
	ld a,h
	cp a,b
	jr nz,cp_lp3		; start over chunk comparison
	ld a,l
	and a,$f0
	cp a,c
	jr nz,cp_lp3		; start over chunk comparison
	xor a				; return 0 to not confuse monitor
	ret				; finished comparing

cp_lp2:	ld hl,(MVADDR+0)	; found mismatch. print hex dump of first range
	call sp_p16
	ld hl,(MVADDR+2)	; print hex dump of second range
	call sp_p16
	ld bc,16
	ld hl,(MVADDR+2)
	add hl,bc
	ld (MVADDR+2),hl
	push hl
	pop de
	ld hl,(MVADDR+0)
	add hl,bc
	ld (MVADDR+0),hl
	call jCON_PRT_NL
	jr cp_lp4

sp_p16:	call jCON_PRINTHWORD	; print address
	ld a," "
	call jCON_PRT_CHAR
	ld b,16
sp_p16a:	ld a,(hl)
	call jCON_PRINTHBYTE	; print memory content
	ld a," "
	call jCON_PRT_CHAR
	inc hl
	djnz sp_p16a
	call jCON_PRT_NL
	ret

endprog	equ $

	output_bin "hex_compare.bin",hex_compare,endprog-hex_compare
	output_intel "hex_compare.hex",hex_compare,endprog-hex_compare
	output_list "hex_compare.lst"

