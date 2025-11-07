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
; Clobbers: A
;***************************************************************************	
CF_INIT:	ld a,$0E			; issue software reset
	out (CFCTL),a
	ld a,$20
	call delay		; reset delay
	ld a,$0A				; exit reset
	out (CFCTL),a
	ld a,$20
	call delay		; reset delay

	xor a
	LD (CF_LBA0), A
	LD (CF_LBA1), A
	LD (CF_LBA2), A
	LD (CF_LBA3), A
	INC A
	LD (CF_SECCNT), A
	ld hl,CFSECT_BUF_V	; = $c000 in preparation for CPM loader which will load data from CF into $c000-$ffff
	ld (CFSECT_BUF),hl	; by default point to this location for CF data buffer
	ld de,CFSECT_BUF_V+1
	ld bc,511
	xor a
	ld (hl),a
	ldir				; zero out CF buffer to not accidentally read old data

	xor a
CF_INIT_LP:	push af	; iteration counter for timeout
	CALL	CF_LP_BUSY
	jr z,CF_INIT_GO
	ld a,$40
	call delay		; reset delay
	pop af
	inc a
	jr nz,CF_INIT_LP
	jr CF_INIT_TOUT

CF_INIT_GO:	pop af	; remove timeout counter off the stack
	ld (IECHECKSUM),a	; stick it somewhere for debugging to see how long the wait was typically
	LD		A,01h						;LD features register to enable 8 bit
	OUT		(CFFEAT),A
	CALL	CF_LP_BUSY
	jr nz,CF_INIT_TOUT				; timeout
	LD		A,0EFh						;Send set features command
	OUT		(CFCMD),A
	CALL	CF_LP_BUSY
	jr nz,CF_INIT_TOUT				; timeout
	call CON_PRT_STR_SP
zoWarnFlow = false
	db 0Dh, 0Ah, "CF Card Initialized", 0Dh, 0Ah, EOS
zoWarnFlow = true
	xor a
	RET

CF_INIT_TOUT:	push af
	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card init timeout ",0
zoWarnFlow = true
	pop af
	call CON_PRINTHBYTE
	call CON_PRT_NL
	xor a
	dec a
	ret

;***************************************************************************
;LOOP_BUSY
;Function: Loops until status register bit 7 (busy) is 0
;***************************************************************************	
CF_LP_BUSY:	push bc
	ld c,0
CF_LP_BUSY_1:	dec c
	jr z,CF_LP_BUSY_X
	ld a,$1
	call delay
	IN		A, (CFSTAT)					;Read status
	AND		010000000b					;Mask busy bit
	JR		NZ,CF_LP_BUSY_1				;Loop until busy(7) is 0
CF_LP_BUSY_X:	pop bc		; we get here either because not busy, A=0 or because timeout, A=$80
	or a					; update flags
	RET

;***************************************************************************
;LOOP_CMD_RDY
;Function: Loops until status register bit 7 (busy) is 0 and drvrdy(6) is 1
;***************************************************************************	
CF_LP_CMD_RDY:	push bc
	ld c,0
CF_LP_CMD_RDY_1:	dec c
	jr z,CF_LP_CMD_RDY_X
	ld a,$1
	call delay
	IN		A,(CFSTAT)					;Read status
	AND		011000000b					;mask off busy and rdy bits
	XOR		001000000b					;we want busy(7) to be 0 and drvrdy(6) to be 1
	JR		NZ,CF_LP_CMD_RDY_1
CF_LP_CMD_RDY_X:	pop bc	; we get here either because not busy & ready, A=0 or because timeout, A=$c0
	or a					; update flags
	RET

;***************************************************************************
;LOOP_DAT_RDY
;Function: Loops until status register bit 7 (busy) is 0 and drq(3) is 1
;***************************************************************************		
CF_LP_DAT_RDY:	push bc
	ld c,0
CF_LP_DAT_RDY_1:	dec c
	jr z,CF_LP_DAT_RDY_X

	ld a,$1
	call delay
	IN		A,(CFSTAT)					;Read status
	AND		010001000b					;mask off busy and drq bits
	XOR		000001000b					;we want busy(7) to be 0 and drq(3) to be 1
	JR		NZ,CF_LP_DAT_RDY_1
CF_LP_DAT_RDY_X:	pop bc		; we get here either because not busy & drq, A=0 or because timeout, A=$88
	or a					; update flags
	RET
	
;***************************************************************************
;CF_RD_CMD
;Function: Gets a sector (512 bytes) into RAM buffer.
;***************************************************************************			
CF_RD_CMD:	CALL 	CF_LP_BUSY
	jr nz,CF_RD_CMD_TOUT				; time out
	LD 		A,(CF_SECCNT)
	OUT 	(CFSECCO),A					;Number of sectors at a time (512 bytes)
	CALL 	CF_LP_BUSY
	jr nz,CF_RD_CMD_TOUT				; time out
	CALL	CF_SETUP_LBA
	CALL	CF_LP_CMD_RDY				;Make sure drive is ready for command
	jr nz,CF_RD_CMD_TOUT				; time out
	LD		A,020h						;Prepare read command
	OUT		(CFCMD),A					;Send read command
	CALL	CF_LP_DAT_RDY				;Wait until data is ready to be read
	jr nz,CF_RD_CMD_TOUT				; time out
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JR		Z,CF_RD_NOERR			; no error, read data
	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card read error: ",0
zoWarnFlow = true
	in A,(CFERR)						; read error code
	call CON_PRINTHBYTE
	call CON_PRT_NL
	jr CF_RD_CMD				;Try again if error
CF_RD_NOERR:	LD 		HL,(CFSECT_BUF)
	LD 		B,0							;read 256 words (512 bytes per sector)
CF_RD_SECT:	CALL	CF_LP_DAT_RDY	
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
	
CF_RD_CMD_TOUT:	push af
	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card read timeout ",0
zoWarnFlow = true
	pop af
	call CON_PRINTHBYTE
	call CON_PRT_NL
	xor a
	dec a		; return $ff
	ret

;***************************************************************************
;CF_WR_CMD
;Function: Puts a sector (512 bytes) from RAM buffer disk buffer and to the disk.
;***************************************************************************			
CF_WR_CMD:	LD A,(CF_SECCNT)
	OUT 	(CFSECCO),A					;Number of sectors at a time (512 bytes)
	CALL 	CF_LP_BUSY
	CALL	CF_SETUP_LBA
	push bc
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
CF_WR_SECT_L:	LD 		A,(HL)
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

CF_WR_CMD_TOUT:	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card write timeout ",0
zoWarnFlow = true
	ld a,c
	call CON_PRINTHBYTE
	call CON_PRT_NL
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
;	CALL 	CF_LP_BUSY
	ld a,(CF_LBA1)
	OUT (CFLBA1), A
;	CALL 	CF_LP_BUSY
	ld a,(CF_LBA2)
	OUT (CFLBA2), A
;	CALL 	CF_LP_BUSY
	ld a,(CF_LBA3)
	and a,$0f
	or a,$e0
	OUT (CFLBA3), A
;	CALL 	CF_LP_BUSY
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

CF_SETUP_PART_LAST: call CON_PRT_STR_SP
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

CF_SYSLD:	push hl
	ld hl,1		; we presume 0th sector was already loaded to $c000 during boot
	ld (CF_LBA0),hl		; so load starting from 1st CF sector
	ld hl,$c200		; and into $c200
	ld (CFSECT_BUF),hl
	ld a,1
	ld (CF_SECCNT),a
CF_SYSLD_LOOP:	call CF_RD_CMD
	or a
	jr nz,CF_SYSLD_ERR
	ld hl,(CFSECT_BUF)
	ld bc,$0200
	add hl,bc
	ld (CFSECT_BUF),hl
	ld a,h
	cp a,$00			; keep going till loaded to $ffff
	jr z,CF_SYSLD_END
	ld hl,(CF_LBA0)
	inc hl
	ld (CF_LBA0),hl
	jr CF_SYSLD_LOOP

CF_SYSLD_END: call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF SYSLD success ",$0d,$0a,0
zoWarnFlow = true
	pop hl
	xor a
	ret

CF_SYSLD_ERR:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF SYSLD error ",$0d,$0a,0
zoWarnFlow = true
	pop hl
	xor a
	dec a
	ret

CF_BOOT:	call CF_INIT		; initialize CF
	ret nz		; if error, just go to main loop
	call 	CF_RD_CMD
	jp nz,CF_READ_ERR
	ld hl,(CFSECT_BUF)
	ld a,(hl)
	cp a,"Z"			; check if first byte = "Z"
	jr nz,CF_SECT_ERR_MBR	; no, invalid CF
	inc hl
	ld a,(hl)
	cp a,$80			; check if 2nd byte = $80
	jr z,CF_MBR_TEST	; yes, magic present, should be valid CF
CF_SECT_ERR_MBR:	call CON_PRT_STR_SP
zoWarnFlow = false
	db "CF card no magic in MBR",$0d,$0a,0
zoWarnFlow = true
	ret

CF_MBR_TEST:	inc hl
	ld a,(hl)
	cp a,0
	jp z,CF_LD_SYS		; if (CFSECT_BUF + 2) = 0 then no code to run, but we have valid CF signature, so load system sectors
	cp a,$ff
	jp z,CF_LD_SYS		; if (CFSECT_BUF + 2) = $ff then no code to run, but we have valid CF signature, so load system sectors

	; otherwise presume this may be a valid code that can be jumped to
	call CON_PRT_STR_SP	; see if user wants to jump to it
zoWarnFlow = false
	db "CF MBR executable found at ",0
zoWarnFlow = true
	call CON_PRINTHWORD

	ld hl,(CFSECT_BUF)	; calculate location of label
	ld bc,$01a0
	add hl,bc
	ld a,(hl)
	cp a," "			; if (boot_dest + $1fe0) < " " then no label to print
	jr c,ask_run
	cp a,$7f			; if (boot_dest + $1fe0) > "" then no label to print
	jr nc,ask_run
	call jCON_PRT_STR_SP	; otherwise print label
zoWarnFlow = false
	db $0d,$0a,"Found label: ",0
zoWarnFlow = true
	call jCON_PRT_STR

; here HL points to 0 terminating bootloader label
	inc hl
	ld a,(hl)
	cp $aa	; if $aa follows label, then skip asking whether to run and autorun
	jr z,gohl

ask_run:	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Run it? (y/n/q) ",$0d,$0a,0
zoWarnFlow = true
	call CON_GET_CHAR
	cp a,"N"
	jr z, CF_LD_SYS
	cp a,"Y"
	ret nz
gohl:	ld hl,(CFSECT_BUF)	; jump target may have gotten overwritten in HL during checks
	inc hl
	inc hl
	call jphl
	ret		; after executing code from CF go to main loop. Unless code from CF does something else

CF_LD_SYS:		call CON_PRT_STR_SP	; see if user wants to jump to it
zoWarnFlow = false
	db "CF Loading SYS sectors",0
zoWarnFlow = true
	ld hl,(CFSECT_BUF)	; save this in HL now because call that follows will destroy it
	call CF_SYSLD		; load rest of CF sectors into $c200-$ffff, this call destroys value of CFSECT_BUF
	ret nz		; if failed loading system, jump to main monitor loop
	ld bc,$0200
	add hl,bc
	ld a,(hl)
	cp a,0			; if (CFSECT_BUF + $0200) = 0 then no code to run
	ret z
	cp a,$ff			; if (CFSECT_BUF + $0200) = $ff then no code to run
	ret z		; otherwise presume this may be a valid code that can be jumped to
	call jCON_PRT_STR_SP
zoWarnFlow = false
	db "CF SYS executable found at ",0
zoWarnFlow = true
	call CON_PRINTHWORD
	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Run it? (y/n) ",$0d,$0a,0
zoWarnFlow = true
	call CON_GET_CHAR
	cp a,"N"
	jr z, CF_LD_PART
	cp a,"Y"
	ret nz
	call jphl
	ret		; after executing code from CF go to main loop. Unless code from CF does something else

CF_LD_PART: ld hl,0	; lastly check if active partition has code to run and offer to run it
	ld (CF_LBA0),hl
	ld hl,CFSECT_BUF_V
	ld (CFSECT_BUF),hl
	ld a,1
	ld (CF_SECCNT),a
	call CF_RD_CMD		;get 0 sector
	call CF_SETUP_PART
	ret nz		; skip to monitor if failed to find partition
	call CF_RD_CMD		;get first sector of active partition

	ld hl,(CFSECT_BUF)
	ld bc,$003e
	add hl,bc
	ld a,(hl)
	cp a,"Z"			; check if first byte = "Z"
	jr nz,CF_SECT_ERR_PART	; no, invalid CF
	inc hl
	ld a,(hl)
	cp a,$80			; check if 2nd byte = $80
	jr z,CF_SECT_TEST	; yes, magic present, should be valid CF
CF_SECT_ERR_PART:	call CON_PRT_STR_SP
zoWarnFlow = false
	db "CF card no magic in partition",$0d,$0a,0
zoWarnFlow = true
	ret

CF_SECT_TEST:	inc hl
	ld a,(hl)
	cp a,0
	ret z		; if (CFSECT_BUF + 2) = 0 then no code to run, jump to monitor
	cp a,$ff
	ret z		; if (CFSECT_BUF + 2) = $ff then no code to run, jump to monitor

	; otherwise presume this may be a valid code that can be jumped to
	call CON_PRT_STR_SP	; see if user wants to jump to it
zoWarnFlow = false
	db "CF active partition with executable found at ",0
zoWarnFlow = true
	call CON_PRINTHWORD
	call CON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"Run it? (y/n) ",$0d,$0a,0
zoWarnFlow = true
	call CON_GET_CHAR
	cp a,"Y"
	ret nz	; if not "Y", go to monitor
	call jphl
	ret		; after executing code from CF go to main loop. Unless code from CF does something else


CF_READ_ERR:	call CON_PRT_STR_SP
zoWarnFlow = false
	db "CF card read error ",$0d,$0a,0
zoWarnFlow = true
	ret


