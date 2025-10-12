;***************************************************************************
;  PROGRAM:			CF_test        
;  PURPOSE:			Subroutines for a CF Card
;  ASSEMBLER:		TASM 3.2        
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	19 June 15
;***************************************************************************

	include "jump.inc"

	org $8000

;***************************************************************************
;CF_READ
;Function: Read sector 0 into RAM buffer.
;***************************************************************************	

CF_READ:	LD 		HL, CF_MSG1					;Print some messages 
	CALL    jCON_PRT_STR
	CALL	CF_MKMS2
	LD		HL, MSGBUF
	CALL    jCON_PRT_STR
	CALL	jCF_RD_CMD
	jr nz,CF_READ_ERR
	CALL	CF_MKMS3
	LD		HL, MSGBUF
	CALL    jCON_PRT_STR
	RET
	
CF_READ_ERR:call jCON_PRT_STR_SP
zoWarnFlow = false
	db "CF card read error ",$0d,$0a,0
zoWarnFlow = true
	ret


CF_MSG1:  DEFB 0Dh, 0Ah, "CF Card Read", 0Dh, 0Ah, EOS

CF_MSG21: DEFB "Reading sector "
CF_MSG2h: DEFB "00000000"
CF_MSG22: DEFB "h into RAM buffer "
CF_MSG2S: DEFB "0000", 0Dh, 0Ah, EOS
CF_MSG2E: 

CF_MSG31: DEFB "Sector "
CF_MSG3h: DEFB "00000000"
CF_MSG32: DEFB  "h read...", 0Dh, 0Ah, EOS
CF_MSG3E:

CF_PROMPT: DEFB	"CF> ", EOS

; clear message buffer
CF_CLMSB:	LD		A, " "
	LD		HL, MSGBUF
	LD		(HL), A
	LD		DE, MSGBUF
	INC		DE
	LD		B, 0
	LD		C, ULBUFSIZE
	DEC		C
	LDIR
	RET
	
;CF_MKMSG2
; Function: Construct CF message before reading in MSGBUF
CF_MKMS3:	CALL	CF_CLMSB
	LD		HL, CF_MSG31
	LD		DE, MSGBUF
	LD		BC, CF_MSG3E - CF_MSG31
	LDIR
	LD		HL, MSGBUF + (CF_MSG3h - CF_MSG31)	; first digit position for CF_MSG31 in MSGBUF
	CALL	CFSECDG
	RET
	
;CF_MKMSG3
; Function: Construct CF message after reading in MSGBUF
CF_MKMS2:	CALL	CF_CLMSB
	LD		HL, CF_MSG21
	LD		DE, MSGBUF
	LD		BC, CF_MSG2E - CF_MSG21
	LDIR
	LD		HL, MSGBUF + (CF_MSG2h - CF_MSG21)	; first digit position for CF_MSG21 in MSGBUF
	CALL    CFSECDG
	LD		HL, MSGBUF + (CF_MSG2S - CF_MSG21)	; first digit position for CF_MSG21 in MSGBUF
	CALL    CFSEBA
	
	RET

	
CFSEBA:	LD		A,(CFSECT_BUF+1)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CFSECT_BUF)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL

	RET

CFSECDG:	LD		A,(CF_LBA3)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA2)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA1)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA0)
	PUSH	AF
	CALL	jCON_SHFTNIB
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jCON_NIB2CHAR
	LD		(HL), A
	
	RET
	
;***************************************************************************
;CF_WRITE
;Function: Write a sector from RAM buffer.
;***************************************************************************	

CF_WRITE:	LD 		A,(CF_SECCNT)
	OUT 	(CFSECCO),A					;Number of sectors at a time (512 bytes)
	CALL 	jCF_LP_BUSY
	CALL	jCF_WR_CMD

	RET
	

;***************************************************************************
;CF_ID_CMD
;Function: Issue the Identify Drive command and read the response into the data buffer
;***************************************************************************
CF_MSGID:	DEFB 0Dh, 0Ah, "CF Card Identify Drive", 0Dh, 0Ah, EOS

CF_ID_CMD:	LD		HL, CF_MSGID
	CALL    jCON_PRT_STR
	CALL 	jCF_LP_BUSY
	CALL	jCF_LP_CMD_RDY				;Make sure drive is ready for command
	LD		A,0ECh						;Prepare ID drive command
	OUT		(CFCMD),A					;Send ID drive command
	CALL	jCF_LP_DAT_RDY				;Wait until data is ready to be read
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JR		NZ,CF_ID_CMD				;Try again if error
	LD 		HL,(CFSECT_BUF)
	LD 		B,0							;read 256 words (512 bytes per sector)
CF_ID1:
	CALL	jCF_LP_DAT_RDY	
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
	CALL	jCF_LP_DAT_RDY
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
	DJNZ 	CF_ID1
	RET

;***************************************************************************
;CF_RD_CMD
;Function: Gets a sector (512 bytes) into RAM buffer.
;***************************************************************************			
CF_RD_CMD:call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF_LP_CMD_RDY",0
zoWarnFlow = true

	LD 		HL,(CFSECT_BUF)
	LD 		BC,0				;read 256 words (512 bytes per sector), also zero error counter

	CALL	jCF_LP_CMD_RDY				;Make sure drive is ready for command
	or a
	call nz,CF_RD_CMD_TOUT			; time out
	LD		A,020h			;Prepare read command
	OUT		(CFCMD),A			;Send read command
call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF_LP_DAT_RDY 1",$0d,$0a,0
zoWarnFlow = true
CF_RD_LP_DAT_RDY:	CALL	jCF_LP_DAT_RDY				;Wait until data is ready to be read
	or a
	call nz,CF_RD_CMD_TOUT				; time out
	jr nz,CF_RD_LP_DAT_RDY
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JR		Z,CF_RD_NOERR			; no error, read data
	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card read error: ",0
zoWarnFlow = true
	in A,(CFERR)						; read error code
	call jCON_PRINTHBYTE
	call jCON_PRT_NL
	jr CF_RD_CMD				;Try again if error

CF_RD_NOERR:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db "Reading",$0d,$0a,0
zoWarnFlow = true

CF_RD_SECT:	CALL	jCF_LP_DAT_RDY	
	or a
	call nz,CF_RD_CMD_TOUT				; time out
	jr nz,CF_RD_SECT
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
CF_RD_SECT1:	CALL	jCF_LP_DAT_RDY
	or a
	call nz,CF_RD_CMD_TOUT				; time out
	jr nz,CF_RD_SECT1
	IN 		A,(CFDATA)					;get byte of ide data	
	LD 		(HL),A
	INC 	HL
	DJNZ 	CF_RD_SECT
	xor a						; zero A to indicate success
	RET
	
CF_RD_CMD_TOUT:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db "CF card read timeout ",0
zoWarnFlow = true
	call jCON_PRINTHBYTE
	ld a," "
	call jCON_PRT_CHAR
	call jCON_PRINTHWORD
 	ld a," "
	call jCON_PRT_CHAR
	xor a
	sub b
	call jCON_PRINTHBYTE
	ld a," "
	call jCON_PRT_CHAR
	ld a,c
	call jCON_PRINTHBYTE
	ld a,$0d
	call jCON_PRT_CHAR
	dec c
	jr z,CF_RD_CMD_TOUT_X
CF_RD_CMD_TOUT_R:	xor a
	dec a		; return $ff
	ret

CF_RD_CMD_TOUT_X:	pop bc		; pop calling address from within CF_RD_CMD
	jr CF_RD_CMD_TOUT_R


;***************************************************************************	
;CF_INIT
;Function: Initialize CF to 8 bit data transfer mode
; Clobbers: A
;***************************************************************************	
CF_INIT:	ld a,$0E			; issue software reset
	out (CFCTL),a
	ld a,$0A				; exit reset
	out (CFCTL),a
	CALL	jCF_LP_BUSY
	or a
	jr nz,CF_INIT_TOUT				; timeout
	LD		A,01h						;LD features register to enable 8 bit
	OUT		(CFFEAT),A
	CALL	jCF_LP_BUSY
	or a
	jr nz,CF_INIT_TOUT				; timeout
	LD		A,0EFh						;Send set features command
	OUT		(CFCMD),A
	CALL	jCF_LP_BUSY
	or a
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
	ld (hl),a
	ld de,hl
	inc de
	ld bc,$01ff
	ldir
	call jCON_PRT_STR_SP
zoWarnFlow = false
	db 0Dh, 0Ah, "CF Card Initialized", 0Dh, 0Ah, EOS
zoWarnFlow = true
	RET

CF_INIT_TOUT:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db $0d,$0a,"CF card init timeout",$0d,$0a,0
zoWarnFlow = true
	ret

CF_RD_32k:	ld hl,0
	ld (CF_LBA0),hl
	ld hl,$5000
	ld (CFSECT_BUF),hl
	ld a,1
	ld (CF_SECCNT),a
CF_RD_32k_LOOP:	call	jCF_RD_CMD
	or a
	jr nz,CF_RD_32k_ERR
	ld hl,(CFSECT_BUF)
	ld bc,$0200
	add hl,bc
	ld (CFSECT_BUF),hl
	ld a,h
	cp a,$d0
	jr z,CF_RD_32k_END
	ld hl,(CF_LBA0)
	inc hl
	ld (CF_LBA0),hl
	jr CF_RD_32k_LOOP

CF_RD_32k_END: call jCON_PRT_STR_SP
zoWarnFlow = false
	db "CF card read success ",$0d,$0a,0
zoWarnFlow = true
	xor a
	ret


CF_RD_32k_ERR:	call jCON_PRT_STR_SP
zoWarnFlow = false
	db "CF card read error ",$0d,$0a,0
zoWarnFlow = true
	xor a
	dec a
	ret



endprog	equ $

	output_bin "CF_test.bin",CF_READ,endprog-CF_READ    ; 
	output_intel "CF_test.hex",CF_READ,endprog-CF_READ    ;
	output_list "CF_test.lst"
	
