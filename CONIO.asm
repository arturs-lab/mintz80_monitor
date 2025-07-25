;***************************************************************************
;  PROGRAM:			CONIO       
;  PURPOSE:			Subroutines for console I/O
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	19 May 15
;***************************************************************************

;***************************************************************************
;PRINT_STRING
;Function: Prints string to terminal program. Start in HL
;***************************************************************************
PRINT_STRING:
        CALL    UART_PRNT_STR
        RET
        
;***************************************************************************
;GET_CHAR
;Function: Get upper case ASCII character from user into Accumulator
;***************************************************************************
GET_CHAR:	CALL    UART_RX         ;Get char into Acc
        CALL    TO_UPPER        ;Character has to be upper case
        RET
        
;***************************************************************************
;PRINT_CHAR
;Function: Get upper case ASCII character from Accumulator to UART
;***************************************************************************
PRINT_CHAR:	CALL UART_TX    ;Echo character to terminal

        RET
        
; Optional print char
OPRINTCHAR:
        LD      C, A
        LD      A, (MUTE)
        CP      MUTEON  ; compare with 1=true
        JR      Z, PRTSKIP
        LD      A, C
        CALL    PRINT_CHAR

PRTSKIP:
        LD      A, C
        RET

;***************************************************************************
;TO_UPPER
;Function: Convert character in Accumulator to upper case 
;***************************************************************************
TO_UPPER:       
        CP      "a"     ; Nothing to do if not lower case
        RET     C
        CP      "z" + 1 ; > "z"?
        RET     NC      ; Nothing to do, either
        AND     5Fh     ; Convert to upper case
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
        
;***************************************************************************
;PRINT_NEW_LINE
;Function: Prints carriage return and line feed
;***************************************************************************			
PRINT_NEW_LINE:
        PUSH    af
        LD      A, $0d
        CALL    PRINT_CHAR
        LD      A, $0a
        CALL    PRINT_CHAR
        POP     af
        RET
        
;***************************************************************************
;CHAR_ISHEX
;Function: Checks if value in A is a hexadecimal digit, C flag set if true
;***************************************************************************		
CHAR_ISHEX:         
                                    ;Checks if Acc between "0" and "F"
        CP      "F" + 1         ;(Acc) > "F"? 
        RET     NC              ;Yes - Return / No - Continue
        CP      "0"             ;(Acc) < "0"?
        JP      NC,CIH1         ;Yes - Jump / No - Continue
        CCF                     ;Complement carry (clear it)
        RET
CIH1:       
                                ;Checks if Acc below "9" and above "A"
        CP      "9" + 1         ;(Acc) < "9" + 1?
        RET     C               ;Yes - Return / No - Continue (meaning Acc between "0" and "9")
        CP      "A"             ;(Acc) > "A"?
        JP      NC,CIH2         ;Yes - Jump / No - Continue
        CCF                     ;Complement carry (clear it)
        RET
CIH2:        
                                ;Only gets here if Acc between "A" and "F"
        SCF                     ;Set carry flag to indicate the char is a hex digit
        RET
        
;***************************************************************************
;GET_HEX_NIBBLE
;Function: Translates char to HEX nibble in bottom 4 bits of A
;***************************************************************************
GETHEXNIB:      
        CALL    GET_CHAR
        CALL    CHAR_ISHEX      ; Is it a hex digit?
        JP      NC,NONHEXNIB    ; Yes - Continue / No - Exit
        CALL    OPRINTCHAR

        CP      "9" + 1         ; Is it a digit less or equal "9" + 1?
        JP      C,IS_NUM        ; Yes - Jump / No - Continue
        SUB     07h             ; Adjust for A-F digits
IS_NUM:                
        SUB     "0"             ; Subtract to get nib between 0->15
        AND     0Fh             ; Only return lower 4 bits
        RET
NONHEXNIB:                      ; Allow exit on wrong char
        LD      A, E_NOHEX
        LD      (ERRFLAG), A    ; Error flag
        RET

;***************************************************************************
;GET_HEX_BTYE
;Function: Gets HEX byte into A
;Uses: AF, D
;***************************************************************************
GETHEXBYTE:
        CALL    GETHEXNIB       ; Get high nibble
        PUSH	DE
        PUSH	AF
        LD	A, (ERRFLAG)
        CP	E_NONE
        JR	NZ, GHB_ERR
        POP	AF
        RLC     A               ; Rotate nibble into high nibble
        RLC     A
        RLC     A
        RLC     A
        LD      D,A             ; Save upper four bits
        CALL    GETHEXNIB       ; Get lower nibble
        PUSH	AF
        LD	A, (ERRFLAG)
        CP	E_NONE
        JR	NZ, GHB_ERR  
        POP	AF          
        OR      D               ; Combine both nibbles
        POP	DE
        RET
GHB_ERR:
        POP	AF
        POP	DE
        RET

;***************************************************************************
;GET_HEX_WORD
;Function: Gets two HEX bytes into HL
;Uses: AF
;***************************************************************************
GETHEXWORD:
        CALL    GETHEXBYTE	;Get high byte
        PUSH	AF
        LD		A, (ERRFLAG)
        CP		E_NONE
        JR		NZ, GHW_ERR
        POP		AF
        LD		H,A
        CALL    GETHEXBYTE    	;Get low byte
        PUSH	AF
        LD		A, (ERRFLAG)
        CP		E_NONE
        JR		NZ, GHW_ERR
        POP     AF
        LD      L,A
        RET
GHW_ERR:
        POP		AF
        RET
        
;***************************************************************************
;NIB2CHAR
;Function: Converts the lower nibble in A into a HEX character
;***************************************************************************
NIB2CHAR:
        AND     0Fh             	;Only low nibble in byte
        ADD     A,"0"             	;Adjust for char offset
        CP      "9" + 1         	;Is the hex digit > 9?
        JR      C,N2C1				;Yes - Jump / No - Continue
        ADD     A,"A" - "0" - 0Ah 	;Adjust for A-F
N2C1:
        RET

;***************************************************************************
;SHFTNIB
;Function: Shift the upper nibble to the lower position
;***************************************************************************
SHFTNIB:
        AND     0F0h
        RRCA
        RRCA
        RRCA
        RRCA
        RET
    
;***************************************************************************
;PRINT_HEX_NIB
;Function: Prints a low nibble in hex notation from Acc to the serial line.
;***************************************************************************
PRINTHNIB:
        PUSH    AF
        CALL    NIB2CHAR
        CALL    PRINT_CHAR        	;Print the nibble
        POP     AF
        RET
        
;***************************************************************************
;PRINT_HEX_BYTE
;Function: Prints a byte in hex notation from Acc to the serial line.
;***************************************************************************		
PRINTHBYTE:
        PUSH    AF      ;Save registers
        PUSH    DE
        LD      D,A     ;Save for low nibble
        RRCA            ;Rotate high nibble into low nibble
        RRCA
        RRCA
        RRCA
        CALL    PRINTHNIB       ;Print high nibble
        LD      A,D             ;Restore for low nibble
        CALL    PRINTHNIB       ;Print low nibble
        POP     DE
        POP     AF
        RET
        
;***************************************************************************
;PRINT_HEX_WORD
;Function: Prints the four hex digits of a word to the serial line from HL
;***************************************************************************
PRINTHWORD:     
;       PUSH    HL
        PUSH    AF
        LD      A,H
        CALL    PRINTHBYTE      ;Print high byte
        LD      A,L
        CALL    PRINTHBYTE      ;Print low byte
        POP     AF
;       POP     HL
        RET

;***************************************************************************
;CHAR TO NIBBLE
;Transforms the HEX-character in A to a value fitting in the lower nibble
;***************************************************************************
CHAR2NIB:
        SUB     "0"
        CP      0ah
        JR      C, C2N_DONE
        SUB     007h
        CP      010h			; need to deduct 20h if it was lower case letter
        JR      C, C2N_DONE
        SUB     020h
C2N_DONE:
        AND     0Fh
        RET

        
PRTBIT:
        JR      C, PB0
        LD      A, "1"
        JR      PBPRT
        
PB0:
        LD      A, "0"
        JR      PBPRT 
        
PBPRT:
        CALL    PRINT_CHAR
        RET

;***************************************************************************
;PRT8BIT
;Function: Special case: print the MPF"s user flag bits
;***************************************************************************
        
PRT8BIT:
        LD      C, A
        LD      B, 8
P8B1:
        RLC     A
        LD      C, A
        CALL    PRTBIT
        DEC     B
        JR      Z, P8B_RET
        LD      A, C
        JR      P8B1

P8B_RET:        
        RET
        
;TWO CHARS TO BYTE
; convert the (hex) char at (HL) and the next to a byte in A. On exit
; HL points to the next, ininterpreted character.
CHARS2BYTE:
        PUSH    BC
        LD      A, (HL)
        INC     HL
        CALL    CHAR2NIB        ; get upper nibble from char
        RLC     A
        RLC     A
        RLC     A
        RLC     A
        LD      B, A
        LD      A, (HL)
        INC     HL
        CALL    CHAR2NIB        ; get lower nibble from char
        OR      B
        POP     BC
        RET

;***************************************************************************
; GETSIZE
; Calculate the size, being End address - Start address
; Start address in MVADDR + MVADDR+1, End address in MVADDR+2 + MVADDR+3
; Size will be in MVADDR+4 + MVADDR+5
;***************************************************************************
;GETSIZE:
;        LD      A, (MVADDR+2)   ; End LSB
;        LD      HL, MVADDR+0    ; (Start LSB)
;        SUB     (HL)
;        PUSH    AF              ; keep Carry flag
;        LD      (MVADDR+4), A   ; range LSB
;        LD      A, (MVADDR+3)   ; End MSB
;        LD      HL, MVADDR+1    ; (Start MSB)     
;        SUB     (HL)
;        LD      (MVADDR+5), A   ; range MSB
;        POP     AF
;        JR      NC, _MTNC
;        DEC     (HL)            ; Correct MSB
;_MTNC
;        RET
