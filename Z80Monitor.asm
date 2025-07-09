;***************************************************************************
;  PROGRAM:			Z80 Monitor        
;  PURPOSE:			ROM Monitor Program
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook. Extended MPF-I version: F.J. Kraan
;  CREATE DATE :	05 May 15 / 2022-03-28
;***************************************************************************

VERSMYR:    EQU     "1"
VERSMIN:    EQU     "0"

            INCLUDE "CONSTANTS.asm" ; copy or edit one of the 
                                  ; CONSTANTS-aaaa-pp.asm files to
                                  ; CONSTANTS.asm
SCAN:        EQU     005FEh

;ROM_BOTTOM:  EQU    0F000h		; Bottom address of ROM
ROM_TOP:     EQU    ROM_BOTTOM + 01FFFh		; Top address of ROM

;RAM_BOTTOM:  EQU    01800h		; Bottom address of RAM
;RAM_TOP:     EQU    RAM_BOTTOM + 1FFFh		; Top address of RAM	
RAM_TOP:     EQU    $ffff

;UART_BASE:  EQU     0E0h        ; Base port address, DART uses 4 ports

MONVARS	EQU	RAM_TOP - $1FF	; SP goes at the top of memory. Put monitor vars and buffers 511 bytes below it
MPFMON:     EQU    0000h
ASCDMPBUF:  EQU    MONVARS + 0h      ;Buffer to construct ASCII part of memory dump
ASCDMPEND:  EQU    MONVARS + 10h     ;End of buffer, fill with EOS
DMPADDR:    EQU    MONVARS + 11h     ;Last dump address
MVADDR:     EQU    MONVARS + 12h     ; 6 bytes: start-address, end-address, dest-address or fill-value (23, 24, 25, 26, 27, 28)
ERRFLAG:    EQU    MONVARS + 18h     ; Location to store 
MUTE:       EQU    MONVARS + 19h     ; 0 - print received chars, 1 - do not print received chars
ULSIZE:     EQU    MONVARS + 1Ah     ; actual size of current/last hex-intel message
IECHECKSUM: EQU    MONVARS + 1Bh        ; hex-intel record checksum
IECADDR:    EQU    MONVARS + 1Ch        ; hex-intel record address (2 bytes)
IERECTYPE:  EQU    MONVARS + 1Eh        ; hex-intel record type
DEBUG:      EQU    MONVARS + 1Fh
MTPHFLAG:	 EQU    MONVARS + 1Fh     ; Phase counter: phase 1 doesn't check old value (being unknown)
RX_READ_P:  EQU    MONVARS + 20h     ; read pointer
RX_WRITE_P: EQU    MONVARS + 22h     ; write pointer
CHKSUM_C:   EQU    MONVARS + 24h     ; uses 3 bytes
CF_SECCNT:  EQU    MONVARS + 27h 
CF_LBA0:    EQU    MONVARS + 28h
CF_LBA1:    EQU    MONVARS + 29h
CF_LBA2:    EQU    MONVARS + 2Ah
CF_LBA3:    EQU    MONVARS + 2Bh
UPLOADBUF:  EQU    MONVARS + 2Ch     ; Buffer for hex-intel upload. Allows up to 32 bytes (20h) per line.
ULBUFSIZE:  EQU    50h                  ; a 20h byte hex-intel record use 75 bytes...
ULBEND:     EQU    UPLOADBUF + ULBUFSIZE
MSGBUF:     EQU    UPLOADBUF

; Error codes intel Hex record
E_NONE:     EQU    00h
E_NOHEX:    EQU    01h			; input char not 0-9, A-F
E_PARAM:    EQU    02h			; inconsistent range; start > end
E_BUFSIZE:  EQU    03h			; size larger than buffer
E_HITYP:    EQU    04h			; unsupported hex-intel record type
E_HICKSM:   EQU    05h			; hex-intel record checksum error
E_HIEND:    EQU    06h			; hex-intel end record type found

HI_DATA:    EQU    00h
HI_END:     EQU    01h

ESC:        EQU    01Bh		; 
EOS:        EQU    000h		; End of string
MUTEON:     EQU    001h
LF:         EQU    00Ah
CR:         EQU    00Dh


            ORG ROM_BOTTOM

ROUTINES:
R_MAIN:     JP      MAIN            ; init DART and starts command loop
R_U_INIT:   JP      UART_INIT       ; configures DARTchannel B 
R_PRT_NL:   JP      PRINT_NEW_LINE  ; sends a CR LF
R_PRT_STR:  JP      PRINT_STRING    ; sends a NULL terminated string
            DEFS    3   ; spare  entries
            DEFS    3
            DEFS    3
            DEFS    3
            
            ORG ROM_BOTTOM + 24     ; room for eight routine entries
;***************************************************************************
;MAIN
;Function: Entrance to user program
;***************************************************************************
MAIN:
	di
	ld a,$7b	; disable wdt
	out ($f0),a
	ld a,$b1
	out ($f1),a
	ld sp,RAM_TOP
;	call memmap_init
	ld a,$20
	call delay		; looks like Z80 needs this delay to successfully write to IO ports
	ld a,$01		; (SYSCLK MHz/2/(value+1))
	out (turbo),a
	call ayinit
	ld bc,$0200	; bc = duration
	ld a,$06		; a = pitch
	call beep
dio:	ld hl,hellostr
	call PRINT_STRING
	call dmpio
	jr gomain

hellostr:	db $0d,$0a,"Hellorld",$0d,$0a,"CPLD config:",0

gomain:	LD SP,RAM_TOP		;Load the stack pointer for stack operations.
	CALL UART_INIT		;Initialize UART
	CALL PRINT_MON_HDR	;Print the monitor header info
	LD A, 00h
	LD (DMPADDR), A
	LD A, 0FFh		; FF00h and next should result in 0000h
	LD (DMPADDR+1), A
	CALL CLEAR_ERROR
	CALL MON_PRMPT_LOOP	;Monitor user prompt loop
	HALT

;***************************************************************************
;CLEAR_SCREEN
;Function: Clears terminal screen
;***************************************************************************
CLEAR_SCREEN:
	LD HL,MON_CLS
	CALL PRINT_STRING
	RET
			
;***************************************************************************
;RESET_COMMAND
;Function: Software Reset to $0000
;***************************************************************************
RESET_COMMAND:
	JP MPFMON		;Jumps to 0000 (MPF-1 monitor re-entry)	
			
;***************************************************************************
;PRINT_MON_HDR
;Function: Print out program header info
;***************************************************************************
MNMSG1:     DEFB    0DH, 0Ah, "AL80 Computer", 09h, 09h, 09h, "2015 MCook"
MNMSG2:     DEFB    0DH, 0Ah, " adaptation to MPF-1 / Z80 DART", 09h, "2022 F.J.Kraan", 0Dh, 0Ah
            DEFB    "Adaptation to AL80 2025 Artur's Lab", 0Dh, 0Ah
MNMSG3A:    DEFB    "Monitor v", VERSMYR, ".", VERSMIN, ", ROM: ", EOS
MNMSG3B:    DEFB    "h, RAM: ", EOS
MNMSG3C:    DEFB    "h, DART: ", EOS
MNMSG3D:    DEFB    "h", 0Dh, 0AH, 0Dh, 0AH
MONHLP:     DEFB    09h," Input ? for command list", 0Dh, 0AH, EOS
MONERR:     DEFB    0Dh, 0AH, "Error in params: ", EOS

PRINT_MON_HDR:
        CALL    CLEAR_SCREEN        ;Clear the terminal screen
        LD      HL, MNMSG1          ;Print some messages
        CALL    PRINT_STRING
        LD      HL, ROM_BOTTOM
        CALL    PRINTHWORD
        LD      HL, MNMSG3B         ; 2nd part, RAM
        CALL    PRINT_STRING
        LD      HL, RAM_BOTTOM
        CALL    PRINTHWORD
        LD      HL, MNMSG3C         ; 3rd part UART
        CALL    PRINT_STRING
        LD      A, UART_BASE
        CALL    PRINTHBYTE
        LD      HL, MNMSG3D         ; 4th part, line ending
        CALL    PRINT_STRING
        RET

;***************************************************************************
;MON_PROMPT
;Function: Prompt user for input
;***************************************************************************			
MON_PROMPT: DEFB ">", EOS

MON_PRMPT_LOOP:
        LD      A, 00h
        LD      (MUTE), A       ; Enables echo of received chars
        LD      HL,MON_PROMPT   ; Print monitor prompt
        CALL    PRINT_STRING
        CALL    GET_CHAR        ; Get a character from user into Acc
        CALL    PRINT_CHAR
        CP      CR
        JR      Z, _MPL_CR
        CALL    PRINT_NEW_LINE  ; Print a new line
_MPL_CR:
        CALL    MON_COMMAND     ; Respond to user input
        CALL    PRINT_NEW_LINE  ; Print a new line
        JR      MON_PRMPT_LOOP

;***************************************************************************
;MON_COMMAND
;Function: User input in accumulator to respond to 
;***************************************************************************
MON_COMMAND:    ; Inserted ERROR_CHK for all commands requiring input
        CALL    CLEAR_ERROR
        CP      "?"
        CALL    Z,HELP_COMMAND
        CP      "D"
        CALL    Z,MDCMD
        CP      "C"
        CALL    Z,CLEAR_SCREEN
        CP      "N"
        CALL    Z,RICOMMAND
        CP      "O"
        CALL    Z,PW_COMMAND
        CP      "P"
        CALL    Z,PSCOMMAND
        CP      "Q"
        CALL    Z,UTERMTST
        CP      "R"
        CALL    Z,RESET_COMMAND
        CP      "M"
        CALL    Z,MOVE_COMMAND
        CP      "F"
        CALL    Z,FILL_COMMAND
        CP      "G"
        CALL    Z,GO_COMMAND
        CP      "K"
        CALL    Z,CL_COMMAND
        CP      "+"
        CALL    Z,NEXTP_COMMAND
        CP      "-"
        CALL    Z,PREVP_COMMAND
        CP      "E"
        CALL    Z,EDIT_COMMAND
        CP      ":"
        CALL    Z,HEXI_COMMAND
        CP      "S"
        CALL    Z,CCKSM_COMMAND
        CP      "T"
        CALL    Z, TRAM_COMMAND
        CP      "Z"
        CALL    Z,REGDUMP_COMMAND
        CALL    ERROR_CHK
        RET
        
UTERMTST:
                        ; micro terminal: scans MPF keyboard and sends ASCII 
                        ; '0'-'F' for the hex keys and '10-1F' for other keys.
        CALL    RX_CHK
        RET     NZ      ; Return on serial received char
        LD      IX, SCTXT
        CALL    SCAN
        CP      010h    ; A - 010h
        JR      C, _UTHEX
        CALL    PRINTHBYTE
        LD      A, " "
        CALL    PRINT_CHAR
        JR      UTERMTST
        
_UTHEX:
        CALL    NIB2CHAR
        CALL    PRINT_CHAR
        LD      A, " "
        CALL    PRINT_CHAR
        JR      UTERMTST
        
;               dpcbafge     ; 7-segment pattern to bit  map
SCTXT   DB      10000111b    ; t
        DB      10101110b    ; S
        DB      10000111b    ; t
        DB      01000011b    ; r.
        DB      10001111b    ; E
        DB      10101110b    ; S

ERROR_CHK:
        LD      A, (ERRFLAG)
        CP      E_NONE
        RET     Z
        LD      HL, MONERR
        CALL    PRINT_STRING
        LD      A, (ERRFLAG)
        CALL    PRINTHBYTE
        CALL    PRINT_NEW_LINE
CLEAR_ERROR:
        PUSH    AF
        LD      A, E_NONE
        LD      (ERRFLAG), A
        POP     AF
        RET
        
        INCLUDE	"UARTDriver.asm"
;        INCLUDE	"DARTDriver.asm"
        INCLUDE	"MONCommands.asm"
        INCLUDE	"CONIO.asm"
;        INCLUDE CFDriver.asm

MON_CLS: DEFB 0Ch, EOS  				;Escape sequence for CLS. (aka form feed) 
		

;        END

; triangular progression delay
; at CPU CLK = 1.333MHz:
; $1a - 9.76ms
; $1b - 10.48ms
; $59 - 100ms
; at CPU CLK = 4MHz:
; $01 - 52us -> 19.23kHz
; $04 - 172us -> 5.814kHz
; $2f - 9.84ms -> 102Hz
; $30 - 10.24ms -> 97.656Hz
; $6d - 49.8ms -> 20Hz
; $78 - 60ms -> 16.666Hz
; $9b - 99.6ms -> 10.04Hz
; $9c - 100.8ms

del00:	ld a,$00		; delay loop
delay:	push af			; count delay
del1:	dec a
	jr nz,del1		; repeat till 0
	pop af
	dec a
	jr nz,delay
	ret

; bc = duration, a = pitch
beep:	push af
bep1:	out (beepr),a
	pop af
	push af
	call delay
	dec bc
	ld a,b
	or c
	jr nz,bep1
	pop af
	ret

dmpio:	ld c,$d0
		ld b,$0
		CALL	PRINT_NEW_LINE
dmpio1:	IN      A, (C)
		CALL	PRINTHBYTE
		inc c
		ld a,c
		cp $e0
		jr nz,dmpio1
		CALL	PRINT_NEW_LINE
		ret

memmap_init:	in a,(beepr)	; unlock memmap
	ld a,0		; init mem map
	out ($d8),a
	ld a,1
	out ($d9),a
	out ($da),a
	out ($db),a
	out ($dc),a
	out ($dd),a
	out ($de),a
	out ($df),a
	out (beepr),a	; lock memmap
	ret

endprog	equ $

	output_bin "z80monitor.bin",0,endprog    ; 
	output_intel "z80monitor.hex",0,endprog    ;
	output_list "z80monitor.lst"
