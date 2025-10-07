;***************************************************************************
;  PROGRAM:			CF_test        
;  PURPOSE:			Subroutines for a CF Card
;  ASSEMBLER:		TASM 3.2        
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	19 June 15
;***************************************************************************

	include "jump.inc"

	org $4000

;***************************************************************************
;CF_READ
;Function: Read sector 0 into RAM buffer.
;***************************************************************************	

CF_READ:	LD 		HL, CF_MSG1					;Print some messages 
	CALL    jPRINT_STRING
	CALL	CF_MKMS2
	LD		HL, MSGBUF
	CALL    jPRINT_STRING
	CALL 	jCF_LP_BUSY
	LD 		A,(CF_SECCNT)
	OUT 	(CFSECCO),A					;Number of sectors at a time (512 bytes)
	CALL 	jCF_LP_BUSY
	CALL	jCF_SETUP_LBA
	CALL	jCF_RD_CMD
	CALL	CF_MKMS3
	LD		HL, MSGBUF
	CALL    jPRINT_STRING
	RET
	
CF_MSG1:  DEFB 0Dh, 0Ah, "CF Card Read", 0Dh, 0Ah, EOS

CF_MSG21: DEFB "Reading sector "
CF_MSG2h: DEFB "00000000"
CF_MSG22: DEFB "h into RAM buffer...", 0Dh, 0Ah, EOS
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
	
	RET

	
CFSECDG:	LD		A,(CF_LBA3)
	PUSH	AF
	CALL	jSHFTNIB
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA2)
	PUSH	AF
	CALL	jSHFTNIB
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA1)
	PUSH	AF
	CALL	jSHFTNIB
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	
	LD		A,(CF_LBA0)
	PUSH	AF
	CALL	jSHFTNIB
	CALL	jNIB2CHAR
	LD		(HL), A
	INC		HL
	POP		AF
	CALL	jNIB2CHAR
	LD		(HL), A
	
	RET
	
;***************************************************************************
;CF_WRITE
;Function: Write a sector from RAM buffer.
;***************************************************************************	

CF_WRITE:	LD 		A,(CF_SECCNT)
	OUT 	(CFSECCO),A					;Number of sectors at a time (512 bytes)
	CALL 	jCF_LP_BUSY
	CALL	jCF_SETUP_LBA
	CALL	jCF_WR_CMD

	RET
	

;***************************************************************************
;CF_ID_CMD
;Function: Issue the Identify Drive command and read the response into the data buffer
;***************************************************************************
CF_MSGID:	DEFB 0Dh, 0Ah, "CF Card Identify Drive", 0Dh, 0Ah, EOS

CF_ID_CMD:	LD		HL, CF_MSGID
	CALL    jPRINT_STRING
	CALL 	jCF_LP_BUSY
	CALL	jCF_LP_CMD_RDY				;Make sure drive is ready for command
	LD		A,0ECh						;Prepare ID drive command
	OUT		(CFCMD),A					;Send ID drive command
	CALL	jCF_LP_DAT_RDY				;Wait until data is ready to be read
	IN		A,(CFSTAT)					;Read status
	AND		000000001b					;mask off error bit
	JP		NZ,CF_ID_CMD				;Try again if error
	LD 		HL,CFSECT_BUF
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



endprog	equ $

	output_bin "CF_test.bin",CF_READ,endprog-CF_READ    ; 
	output_intel "CF_test.hex",CF_READ,endprog-CF_READ    ;
	output_list "CF_test.lst"
	
