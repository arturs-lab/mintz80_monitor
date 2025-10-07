;***************************************************************************
;  PROGRAM:			CFDriver        
;  PURPOSE:			Subroutines for a CF Card
;  ASSEMBLER:		TASM 3.2        
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	19 June 15
;***************************************************************************


;***************************************************************************
;CF_INIT
;Function: Initialize CF to 8 bit data transfer mode
;***************************************************************************	
CF_INIT:	CALL	CF_LP_BUSY
	jr nz,CF_INIT_TOUT				; timeout
	LD		A,01h						;LD features register to enable 8 bit
	OUT		(CFFEAT),A
	CALL	CF_LP_BUSY
	jr nz,CF_INIT_TOUT				; timeout
	LD		A,0EFh						;Send set features command
	OUT		(CFCMD),A
	CALL	CF_LP_BUSY
	jr nz,CF_INIT_TOUT				; timeout
	xor a
	LD		(CF_LBA0), A
	LD		(CF_LBA1), A
	LD		(CF_LBA2), A
	LD		(CF_LBA3), A
	INC		A
	LD		(CF_SECCNT), A
	ld hl,RAM_TOP - $0fff 		; = $f000 - $f200
	ld (CFSECT_BUF),hl			; by default point to this location for CF data buffer
	LD 		HL,CF_MSG_i					;Print some messages 
	CALL    PRINT_STRING
	RET

CF_INIT_TOUT:	call jUART_PRNT_SP
zoWarnFlow = false
	db $0d,$0a,"CF card init timeout",$0d,$0a,0
zoWarnFlow = true
	ret

CF_MSG_i: DEFB 0Dh, 0Ah, "CF Card Initialized", 0Dh, 0Ah, EOS

;***************************************************************************
;LOOP_BUSY
;Function: Loops until status register bit 7 (busy) is 0
;***************************************************************************	
CF_LP_BUSY:	push bc
	ld c,0
CF_LP_BUSY_1:	dec c
	jr z,CF_LP_BUSY_X
	IN		A, (CFSTAT)					;Read status
	AND		010000000b					;Mask busy bit
	JR		NZ,CF_LP_BUSY_1				;Loop until busy(7) is 0
CF_LP_BUSY_X:	pop bc		; we get here either because not busy, A=0 or because timeout, A=$80
	RET

;***************************************************************************
;LOOP_CMD_RDY
;Function: Loops until status register bit 7 (busy) is 0 and drvrdy(6) is 1
;***************************************************************************	
CF_LP_CMD_RDY:	push bc
	ld c,0
CF_LP_CMD_RDY_1:	dec c
	jr z,CF_LP_CMD_RDY_X
	IN		A,(CFSTAT)					;Read status
	AND		011000000b					;mask off busy and rdy bits
	XOR		001000000b					;we want busy(7) to be 0 and drvrdy(6) to be 1
	JR		NZ,CF_LP_CMD_RDY_1
CF_LP_CMD_RDY_X:	pop bc	; we get here either because not busy & ready, A=0 or because timeout, A=$c0
	RET

;***************************************************************************
;LOOP_DAT_RDY
;Function: Loops until status register bit 7 (busy) is 0 and drq(3) is 1
;***************************************************************************		
CF_LP_DAT_RDY:	push bc
	ld c,0
CF_LP_DAT_RDY_1:	dec c
	jr z,CF_LP_DAT_RDY_X

	IN		A,(CFSTAT)					;Read status
	AND		010001000b					;mask off busy and drq bits
	XOR		000001000b					;we want busy(7) to be 0 and drq(3) to be 1
	JR		NZ,CF_LP_DAT_RDY_1
CF_LP_DAT_RDY_X:	pop bc		; we get here either because not busy & drq, A=0 or because timeout, A=$88
	RET
	
;***************************************************************************
;CF_RD_CMD
;Function: Gets a sector (512 bytes) into RAM buffer.
;***************************************************************************			
CF_RD_CMD:
	CALL	CF_LP_CMD_RDY				;Make sure drive is ready for command
	jr nz,CF_RD_CMD_TOUT				; time out
	LD		A,020h						;Prepare read command
	OUT		(CFCMD),A					;Send read command
	CALL	CF_LP_DAT_RDY				;Wait until data is ready to be read
	jr nz,CF_RD_CMD_TOUT				; time out
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JR		NZ,CF_RD_CMD				;Try again if error
	LD 		HL,(CFSECT_BUF)
	LD 		B,0							;read 256 words (512 bytes per sector)
CF_RD_SECT:
	CALL	CF_LP_DAT_RDY	
	jr nz,CF_RD_CMD_TOUT				; time out
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
	CALL	CF_LP_DAT_RDY
	jr nz,CF_RD_CMD_TOUT				; time out
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
	DJNZ 	CF_RD_SECT
	xor a						; zero A to indicate success
	RET
	
CF_RD_CMD_TOUT:	call jUART_PRNT_SP
zoWarnFlow = false
	db $0d,$0a,"CF card read timeout",$0d,$0a,0
zoWarnFlow = true
	xor a
	dec a		; return $ff
	ret

;***************************************************************************
;CF_WR_CMD
;Function: Puts a sector (512 bytes) from RAM buffer disk buffer and to the disk.
;***************************************************************************			
CF_WR_CMD:	push bc
	ld c,0
CF_WR_CMD_1:	dec c
	jr z,CF_WR_CMD_TOUT

	CALL	CF_LP_CMD_RDY				;Make sure drive is ready for command
	jr nz,CF_WR_CMD_TOUT				; time out

	LD		A,030h						;Prepare write command
	OUT		(CFCMD),A					;Send write buffer command
	CALL	CF_LP_DAT_RDY				;Wait until drive is ready to be written
	jr nz,CF_WR_CMD_TOUT				; time out
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JR		NZ,CF_WR_CMD_1				;Try again if error
	
; add code here to write data
	LD 		HL,(CFSECT_BUF)
	LD 		B,0					;write 256 words (512 bytes per sector)
CF_WR_SECT_L:
	LD 		A,(HL)
	OUT 		(CFDATA),A				;write byte of ide data	
	INC 	HL
;	CALL	CF_LP_DAT_RDY
;	jr nz,CF_WR_CMD_TOUT				; time out
	LD 		A,(HL)
	OUT 		(CFDATA),A				;write byte of ide data	
	INC 	HL
;	CALL	CF_LP_DAT_RDY	
;	jr nz,CF_WR_CMD_TOUT				; time out
	DJNZ 	CF_WR_SECT_L

	pop bc
	xor a						; zero A to indicate success
	ret

CF_WR_CMD_TOUT:	call jUART_PRNT_SP
zoWarnFlow = false
	db $0d,$0a,"CF card write timeout",$0d,$0a,0
zoWarnFlow = true
	pop bc
	xor a
	dec a	; return $ff

	ret

CF_WR_SECT:	push bc
	LD 		B,0					;write 256 words (512 bytes per sector)
	JR CF_WR_SECT_L

;set LBA on SD card to values from monitor vars
CF_SETUP_LBA:	ld a,(CF_LBA0)
	OUT (CFLBA0), A
;	CALL 	jCF_LP_BUSY
	ld a,(CF_LBA1)
	OUT (CFLBA1), A
;	CALL 	jCF_LP_BUSY
	ld a,(CF_LBA2)
	OUT (CFLBA2), A
;	CALL 	jCF_LP_BUSY
	ld a,(CF_LBA3)
	and a,$0f
	or a,$e0
	OUT (CFLBA3), A
;	CALL 	jCF_LP_BUSY
	ret

; find active partition and put its LBA address in monitor vars
CF_SETUP_PART:	push hl
	push bc
	ld hl,(CFSECT_BUF)
	ld bc,$01be				; point to first entry of partition table
CF_SETUP_PART1:	add hl,bc
	ld a,l
	cp a,$fe					; did we go past 4th partition?
	jr z,CF_SETUP_PART_LAST

	ld a,(HL)
	cp a,$80					; is it an active partition?
	jr z,CF_SETUP_PART2
CF_NEXT_PART:	ld bc,$0010
	jr CF_SETUP_PART1

CF_SETUP_PART2:	ld (CF_PART_CUR),hl	; save pointer to currently used partition in MBR
	ld bc,$0008				; point to current partition's address
	add hl,bc
	push de
	ld de,CF_LBA0
	ld bc,$0004
	ldir						; copy it to monvars
	pop de

	xor a
CF_SETUP_X:	pop bc
	pop hl
	ret

CF_SETUP_PART_LAST: call jUART_PRNT_SP
zoWarnFlow = false
	db $0d,$0a,"No valid partition found",$0d,$0a,0
zoWarnFlow = true
	xor a
	dec a
	jr CF_SETUP_X

; point to next partition
CF_PART_NEXT:	push hl
	push bc
	ld hl,(CF_PART_CUR)
	jr CF_NEXT_PART
