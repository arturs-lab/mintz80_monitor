			org $F000

;***************************************************************************
;MEMORY_DUMP_COMMAND
;Function: Print $80 databytes from specified location
;***************************************************************************
MDCMD:
			LD 		HL,MDC_1			;Print some messages 
			CALL    jPRINT_STRING
			
			CALL    jGETHEXWORD			;HL now points to databyte location	
			LD		A, (ERRFLAG)
			CP		E_NONE
			RET		NZ
			LD		(DMPADDR), HL		;Keep address for next/prev.
			PUSH	HL					;Save HL that holds databyte location on stack
			CALL    jPRINT_NEW_LINE		;Print some messages
			CALL    jPRINT_NEW_LINE
			LD 		HL, MDC_3	
			CALL    jPRINT_STRING

			POP		HL					;Restore HL that holds databyte location on stack
MDNXTPR:	LD		C,HEXLINES			;Register C holds counter of dump lines to print
MDLINE:	
			LD		DE,	ASCDMPBUF
			LD		B,16				;Register B holds counter of dump bytes to print
			CALL	jPRINTHWORD			;Print dump line address in hex form
			LD		A," "				;Print spacer
			CALL	jPRINT_CHAR
			DEC		C					;Decrement C to keep track of number of lines printed
MDBYTES:
			LD		A,(HL)				;Load Acc with databyte HL points to
			PUSH AF
			CALL	BYTE_TO_DEC  		;convert byte to decimal
			CALL PRINT_DEC			; print decimal number
			POP AF
			CALL	CHAR2BUF			;Store ASCII char
			LD		A," "				;Print spacer
			CALL	jPRINT_CHAR	
			INC 	HL					;Increase HL to next address pointer
			DJNZ	MDBYTES				;Print 16 bytes out since B holds 16
			
			LD		A," "				;Print spacer
			CALL	jPRINT_CHAR			;
			LD		A, EOS
			LD		(ASCDMPEND), A		;Make sure there is a EOS

			PUSH	HL
			LD		HL, ASCDMPBUF		;Point HL to ASCII buffer
			CALL    jPRINT_STRING		;Print buffer
			POP		HL
			
			LD		B,C					;Load B with C to keep track of number of lines printed
			CALL    jPRINT_NEW_LINE		;Get ready for next dump line
			DJNZ	MDLINE				;Print 16 line out since C holds 16 and we load B with C
			LD		A,EOS				;Load $FF into Acc so MON_COMMAND finishes

			RET

CHAR2BUF:
			CALL	MKPRINT
			LD		(DE), A
			INC		DE
			RET

;***************************************************************************
;MKPRINT
;Function: Make all characters printable by replacing control-chars with "."
;***************************************************************************
LOWPRTV:    EQU         " "
HIGPRTV:    EQU         "~"
MKPRINT:
        CP      LOWPRTV
        JR      C, ADDOT
        CP      HIGPRTV
        JR      NC, ADDOT
        RET
ADDOT:
        LD      A, "."
        RET
        

    ; Assume 'A' holds the byte to convert (0-255)
    ; 'HUND_CNT' is a memory location to store the hundreds digit
    ; 'TEN_CNT' is a memory location to store the tens digit
    ; 'UNI_CNT' is a memory location to store the units digit

BYTE_TO_DEC:
    PUSH BC
    LD B, 100       ; Divisor (100)
    LD C, 0         ; Counter for hundreds digit

SUB_HUNDREDS_LOOP:
    CP B            ; Compare A with 100
    JR C, END_HUNDREDS ; If A < 100, we're done subtracting
    SUB B           ; A = A - 100
    INC C           ; Increment hundreds counter
    JR SUB_HUNDREDS_LOOP

END_HUNDREDS:
    LD B,A          ; save remainder in B
    LD A, C         ; Move hundreds count to A
    LD (HUND_CNT), A ; Store hundreds digit
    LD A,B          ; A now contains the remainder after subtracting hundreds

    LD B, 10        ; Divisor (10)
    LD C, 0         ; Counter for hundreds digit

SUB_TENS_LOOP:
    CP B            ; Compare A with 100
    JR C, END_TENS  ; If A < 100, we're done subtracting
    SUB B           ; A = A - 100
    INC C           ; Increment hundreds counter
    JR SUB_TENS_LOOP

END_TENS:
    LD (UNI_CNT), A ; Store units digit (remainder)
    LD A, C         ; Move tens count to A
    LD (TEN_CNT), A ; Store tens digit

    POP BC
    RET

PRINT_DEC:	LD A,(HUND_CNT)
		ADD A,"0"
		CP "0"
		JR NZ,PR_HUN_NUM
		LD A," "
PR_HUN_NUM:	;PUSH AF
		CALL jPRINT_CHAR
;		POP AF

		CP " "		; was previous digit blank?
		JR Z,PR_TEN_BLNK	; if yes, then process current digit with blanking
		LD A,(TEN_CNT)		; otherwise just print the digit even if 0
		ADD A,"0"
		JR PR_TEN_NUM

PR_TEN_BLNK	LD A,(TEN_CNT)
		ADD A,"0"
		CP "0"
		JR NZ,PR_TEN_NUM
		LD A," "
PR_TEN_NUM:	CALL jPRINT_CHAR

		LD A,(UNI_CNT)	; just print units with no blanking regardless
		ADD A,"0"
		CALL jPRINT_CHAR

		RET

	include "jump.inc"

MDC_1: DEFB "Memory Dump", 0Dh, 0Ah
MDC_2: DEFB "Location to start in 4 digit HEX: ",EOS
MDC_3: DEFB "      0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F", 0Dh, 0Ah, EOS


HUND_CNT:	DB 0
TEN_CNT:	DB 0
UNI_CNT:	DB 0


endprog	equ $

	output_bin "decdump.bin",MDCMD,endprog-MDCMD	; 
	output_intel "decdump.hex",MDCMD,endprog-MDCMD	;
	output_list "decdump.lst"
