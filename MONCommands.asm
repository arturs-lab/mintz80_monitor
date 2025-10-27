;***************************************************************************
;  PROGRAM:			MONCommands        
;  PURPOSE:			Subroutines for all monitor commands
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	06 May 15 / 2021-01-01
;***************************************************************************

;***************************************************************************
;HELP_COMMAND
;Function: Print help dialogue box
;***************************************************************************
HLPMSG1: DEFB "ZMC80 Monitor Command List", 0Dh, 0Ah
HLPMSG2: DEFB "? - view command list", 0Dh, 0Ah
         DEFB "A - address range compare", 0Dh, 0Ah
HLPMSGc: DEFB "C - clear screen", 0Dh, 0Ah
HLPMSGd: DEFB "D - print 100h bytes from specified location", 0Dh, 0Ah
HLPMSGe: DEFB "E - edit bytes in memory", 0Dh, 0Ah
HLPMSGf: DEFB "F - fill memory range with value", 0Dh, 0Ah
HLPMSGg: DEFB "G - jump to memory address", 0Dh, 0Ah
HLPMSGk: DEFB "K - call to memory address", 0Dh, 0Ah
HLPMSGm: DEFB "M - copy bytes in memory", 0Dh, 0Ah
         DEFB "N - read IO port", 0Dh, 0Ah
HLPMSGo: DEFB "O - write byte to output port", 0Dh, 0Ah
HLPMSGp: DEFB "P - print port scan (00-FF)", 0Dh, 0Ah
HLPMSGr: DEFB "R - monitor reset", 0Dh, 0Ah
HLPMSGs: DEFB "S - calculate checksum for memory range", 0Dh, 0Ah
HLPMSGt: DEFB "T - test memory range", 0Dh, 0Ah
HLPMSGz: DEFB "Z - dump user registers (STEP)", 0Dh, 0Ah
HLPMSG8: DEFB "+ - print next block of memory", 0Dh, 0Ah
HLPMSG9: DEFB "- - print previous block of memory", 0Dh, 0Ah
HLPMSGA: DEFB ": - Load hex-intel record", 0DH, 0AH, EOS

HELP_COMMAND:
        LD      HL, HLPMSG1     ;Print some messages
        CALL    CON_PRT_STR
        LD      A, EOS          ;Load $FF into Acc so MON_COMMAND finishes
        RET

;***************************************************************************
;Address range compare
;Function: compare two memory locations in 16 byte chunks
;          print chunks which differ
;***************************************************************************
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

;***************************************************************************
;MEMORY_DUMP_COMMAND
;Function: Print $80 databytes from specified location
;***************************************************************************
MDC_1: DEFB "Memory Dump", 0Dh, 0Ah
MDC_2: DEFB "Location to start in 4 digit HEX: ",EOS
MDC_3: DEFB "      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F", 0Dh, 0Ah, EOS

MDCMD:
			LD 		HL,MDC_1			;Print some messages 
			CALL    CON_PRT_STR
			
			CALL    CON_GETHEXWORD			;HL now points to databyte location	
			LD		A, (ERRFLAG)
			CP		E_NONE
			RET		NZ
			LD		(DMPADDR), HL		;Keep address for next/prev.
			PUSH	HL					;Save HL that holds databyte location on stack
			CALL    CON_PRT_NL		;Print some messages
			CALL    CON_PRT_NL
			LD 		HL, MDC_3	
			CALL    CON_PRT_STR

			POP		HL					;Restore HL that holds databyte location on stack
MDNXTPR:	LD		C,HEXLINES			;Register C holds counter of dump lines to print
MDLINE:	
			LD		DE,	ASCDMPBUF
			LD		B,16				;Register B holds counter of dump bytes to print
			CALL	CON_PRINTHWORD			;Print dump line address in hex form
			LD		A," "				;Print spacer
			CALL	CON_PRT_CHAR
			DEC		C					;Decrement C to keep track of number of lines printed
MDBYTES:
			LD		A,(HL)				;Load Acc with databyte HL points to
			CALL	CON_PRINTHBYTE  		;Print databyte in HEX form 
			CALL	CHAR2BUF			;Store ASCII char
			LD		A," "				;Print spacer
			CALL	CON_PRT_CHAR	
			INC 	HL					;Increase HL to next address pointer
			DJNZ	MDBYTES				;Print 16 bytes out since B holds 16
			
			LD		A," "				;Print spacer
			CALL	CON_PRT_CHAR			;
			LD		A, EOS
			LD		(ASCDMPEND), A		;Make sure there is a EOS

			PUSH	HL
			LD		HL, ASCDMPBUF		;Point HL to ASCII buffer
			CALL    CON_PRT_STR		;Print buffer
			POP		HL
			
			LD		B,C					;Load B with C to keep track of number of lines printed
			CALL    CON_PRT_NL		;Get ready for next dump line
			DJNZ	MDLINE				;Print 16 line out since C holds 16 and we load B with C
			LD		A,EOS				;Load $FF into Acc so MON_COMMAND finishes

			RET

CHAR2BUF:
			CALL	MKPRINT
			LD		(DE), A
			INC		DE
			RET

;***************************************************************************
;MEMORY_MOVE_COMMAND
;Function: Copy data blocks in memory
;***************************************************************************
MVC_1:	DEFB	"Move Data", 0Dh, 0Ah, EOS
MVC_S:	DEFB	"Start Location: ", EOS
MVC_E:	DEFB	"End Location: ", EOS
MVC_D:	DEFB	"Destination Location: ", EOS

MOVE_COMMAND:	LD		HL, MVC_1	; Print some messages
        CALL	CON_PRT_STR
        
        LD		HL, MVC_S
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        LD		(MVADDR), HL
        CALL	CON_PRT_NL
        
        LD		HL, MVC_E
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        LD		(MVADDR+2), HL
        CALL	CON_PRT_NL
        
        LD		HL, MVC_D
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        LD		(MVADDR+4), HL
        CALL	CON_PRT_NL
        
;***************************************************************************
; Adapted copy from MPF-1(B) Monitor
;***************************************************************************
        ld		hl, MVADDR
        call	GETP	; Fix BC contents from address, to size
        jp		c, MERR
        ld		de, (MVADDR+4)
        sbc		hl, de
        jr		nc, MVUP
MVDN:	ex		de, hl
        add		hl, bc
        dec		hl
        ex		de, hl
        ld		hl, (MVADDR+2)
        lddr
        inc		de
        RET
MVUP:		add		hl,de
        ldir
        dec		de
        RET;
MERR:		LD		A, E_PARAM
        LD		(ERRFLAG), A
        RET;

GETP:		ld		e, (hl) ; MVADDR
        inc		hl
        ld		d, (hl) ; MVADDR+1
        inc		hl
        ld		c, (hl) ; MVADDR+2
        inc		hl
        ld		h, (hl) ; MVADDR+3
        ld		l, c
        or		a
        sbc		hl, de
        ld		c, l
        ld		b, h
        inc		bc
        ex		de, hl
        ret	
;***************************************************************************
; End copy from MPF-1(B) Monitor
;***************************************************************************

;***************************************************************************
; Memory Fill Command
; Function: Fill a memory block
;***************************************************************************

MFC_1:	DEFB	"Fill Memory", 0Dh, 0Ah, EOS
MFC_D:	DEFB	"Data value (one byte): ", EOS

FILL_COMMAND:	LD		HL, MFC_1	; Print some messages
        CALL	CON_PRT_STR
        
        LD		HL, MVC_S	; Start msg.
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        LD		(MVADDR), HL	; Start val.
        CALL	CON_PRT_NL
        
        LD		HL, MVC_E	; End msg.
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		(MVADDR+2), HL	; End val.
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        
        LD		DE, (MVADDR)	; Start
        SBC		HL, DE		; Make sure end is past start...
        JR		C, F_ORDERR
        LD		HL, (MVADDR+2)
        CALL	CON_PRT_NL
        
        LD		HL, MFC_D
        CALL	CON_PRT_STR
        CALL	CON_GETHEXBYTE
        LD		(MVADDR+4), A
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        CALL	CON_PRT_NL

        LD		DE, (MVADDR)	; Start
        LD		HL, (MVADDR+2)	; End
        SBC		HL, DE			; Size
        LD		B, H
        LD		C, L
        LD		A, (MVADDR+4)	; Fill value
        LD		HL, (MVADDR)	; First source location
        LD		(HL), A			; seed the fill block
        LD		DE, (MVADDR)	; First dest. location
        INC		DE				; 
        LDIR
        RET
        
F_ORDERR:
        LD		A, E_PARAM
        LD		(ERRFLAG), A
        RET
        
;***************************************************************************
; Next Page Memory Dump Command
; Function: Print the next block of memory
;***************************************************************************

NEXTP_COMMAND:	LD 		HL,MDC_3	
        CALL    CON_PRT_STR
        LD		HL, (DMPADDR)
        INC		H
        LD		(DMPADDR), HL
        JP		MDNXTPR

;***************************************************************************
; Previous Page Memory Dump Command
; Function: Print the previous block of memory
;***************************************************************************

PREVP_COMMAND:	LD 		HL,MDC_3	
        CALL    CON_PRT_STR
        LD		HL, (DMPADDR)
        DEC		H
        LD		(DMPADDR), HL
        JP		MDNXTPR

;***************************************************************************
; Edit Memory Command
; Function: Edit bytes in memory
;***************************************************************************

EDIT_COMMAND:	LD 		HL, MVC_S	; Start msg.
        CALL    CON_PRT_STR
        
        CALL	CON_GETHEXWORD	; Get first address
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        
EDIT_LP:	LD		A, ":"
        CALL	CON_PRT_CHAR
        LD		A, " "
        CALL	CON_PRT_CHAR
        
        LD		A, (HL)		; Print original value
        CALL	CON_PRINTHBYTE
        
        LD		A, ">"
        CALL	CON_PRT_CHAR
        LD		A, " "
        CALL	CON_PRT_CHAR
        
        CALL	CON_GETHEXBYTE
        LD		(MVADDR+4), A
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ

        LD		A, (MVADDR+4)
        LD		(HL), A		; Write new value
        
        CALL	CON_PRT_NL
        INC		HL
        CALL	CON_PRINTHWORD
        JR		EDIT_LP		; Only way out is type a non-hex char...

;***************************************************************************
;PORT_SCAN_COMMAND
;Function: Print $100 databytes from specified location
;***************************************************************************
PSC_1: DEFB "Port Scan", 0Dh, 0Ah, EOS
;PSC_3: DEFB "     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F", 0Dh, 0Ah, EOS

PSCOMMAND:	LD 		HL,PSC_1			;Print some messages 
        CALL    CON_PRT_STR
        
        LD 		HL,MDC_3			;Print some messages 
        CALL    CON_PRT_STR

        LD		BC, 0h
        XOR     A
PS_NEWPL:                   ; Start new line, start with port address
        LD		A, " "
        CALL	CON_PRT_CHAR  ; address - contents separator
        LD      A,C
        CALL	CON_PRINTHBYTE
        LD		A, " "
        CALL	CON_PRT_CHAR  ; address - contents separator
        CALL	CON_PRT_CHAR
        
PS_LOOP:                    ; Print port contents
        IN		A, (C)
        CALL	CON_PRINTHBYTE
        LD		A, " "
        CALL	CON_PRT_CHAR ; inter-port-contents separator
        
        INC		BC
        XOR		A
        ADD		A, B
        JR      NZ, PS_END  ; check for all ports done
        
        LD		A, C
        AND		00Fh	; multiples of 16
        JR      NZ, PS_LOOP	; line not yet full
        
        CALL	CON_PRT_NL
        JR		PS_NEWPL
        
PS_CONT:                    ; continue on same line
        LD		A, " "
        CALL	CON_PRT_CHAR
        JR		PS_LOOP

PS_END:                     ; done all ports
        RET

;***************************************************************************
;READ_FROM_IO_COMMAND
;Function: Print value read from specified IO port
;***************************************************************************
RIC_1: DEFB "IO Read", 0Dh, 0Ah
RIC_2: DEFB "Enter IO address. Only $00nn supported. 'ENTER' ends", 0Dh, 0Ah, EOS

RICOMMAND:        LD 		HL,RIC_1			;Print some messages 
        CALL    CON_PRT_STR
        
RILOOP:        CALL    CON_GETHEXBYTE
        LD      (MVADDR), A             ; Misuse Move address buffer to store port
        LD      A, (ERRFLAG)
        CP      E_NONE
        JR      NZ,RICOUT
        
        LD		A, " "
        CALL	CON_PRT_CHAR
        LD      A, (MVADDR)
        LD      C, A
        LD      B, 0
        IN      A, (C)
        CALL	CON_PRINTHBYTE
        CALL	CON_PRT_NL
        JR      RILOOP
RICOUT:        CALL	CON_PRT_NL
        RET

; untested code, 2023-04-24
;***************************************************************************
; Port Write Command
; Function: Write byte to port
;***************************************************************************

MPW_1:  DEFB    "Write data to port", 0Dh, 0Ah
MPW_P:  DEFB    "Port & data: ", EOS

PW_COMMAND:	LD      HL, MPW_1
        CALL    CON_PRT_STR
        CALL    CON_GETHEXBYTE
        LD      (MVADDR), A             ; Misuse Move address buffer to store port
        LD      A, (ERRFLAG)
        CP      E_NONE
        RET     NZ
        
        LD      A, " "
        CALL    CON_PRT_CHAR
        CALL    CON_GETHEXBYTE
        LD      (MVADDR+1), A
        LD      A, (ERRFLAG)
        CP      E_NONE
        RET     NZ
        
        LD      A, (MVADDR)
        LD      C, A
        LD      A, (MVADDR+1)
        OUT     (C), A
        xor a			; zero A because upon return other commands will be tested for and value in a may trigger them
        RET

;***************************************************************************
; Jump to memory Command
; Function: Execute a program at memory location
;***************************************************************************

MGo_1:	DEFB	"Execute program in memory", 0Dh, 0Ah, EOS

MGo_2:	DEFB	"Memory location: ", EOS

GO_COMMAND:	LD		HL, MGo_1	; Print some messages
        CALL	CON_PRT_STR
        LD		HL, MGo_2	; Print some messages
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ

        JP       (HL)	; Jump
        
;***************************************************************************
; Call to memory Command
; Function: Execute a program at memory location and expect a RET
;***************************************************************************

MCl_1:	DEFB	"Call program in memory", 0Dh, 0Ah, EOS

CL_COMMAND:	LD		HL, MCl_1	; Print some messages
        CALL	CON_PRT_STR
        LD		HL, MGo_2	; Print some messages
        CALL	CON_PRT_STR
        CALL	CON_GETHEXWORD
        LD		A, (ERRFLAG)
        CP		E_NONE
        RET		NZ
        
        LD		DE, MON_COMMAND
        PUSH	DE			; Add a suitable return address to the stack
        
        JP	(HL)
        RET
        
;***************************************************************************
; Checksum generator. Add memory values in a three byte counter. The last
; included location is end point - 1.
; Function: Calculate checksum for address range in three bytes
; Exit: Checksum MSB in A, middle and LSB in HL
;***************************************************************************

CCKSM_COMMAND:	CALL	CON_PRT_STR_SP
zoWarnFlow = false
	DEFB    "Calculate checksum for memory range", 0Dh, 0Ah, "Start location: ", EOS
zoWarnFlow = true
        
	CALL	CON_GETHEXWORD
	LD		A, (ERRFLAG)
	CP		E_NONE
	RET	NZ
	LD      (MVADDR+0), HL
        
	CALL	CON_PRT_STR_SP
zoWarnFlow = false
	DEFB    $0d,$0a,"End location: ", EOS
zoWarnFlow = true
	CALL	CON_GETHEXWORD
	LD		A, (ERRFLAG)
	CP		E_NONE
	RET	NZ
	INC	HL
	LD      (MVADDR+2), HL
        
CCKSM_DO: LD      BC, (MVADDR+0)  ; starting point
	LD      DE, (MVADDR+2)  ; end point
	LD      HL, 0           ; the checksum value
	LD      A, 0
	LD      (CHKSUM_C), A   ; checksum overflow
CCSM_1:                     ; main checksum loop
	LD      A, C
	CP      E
	JR      NZ, CCSM_3      ; on no match in LSB, skip the MSB
	LD      A, B
	CP      D
	JR      Z, CCSM_4       ; MSB matches too
CCSM_3:                     ; still going, add next value to checksum
	LD      A, (BC)
	ADD     A, L
	LD      L, A
	JR      NC, CCSM_2      ; check carry in checksum LSB
	LD      A, H
	ADD     A, 1
	LD      H, A
	JR      NC, CCSM_2
	LD      A, (CHKSUM_C)
	INC     A
	LD      (CHKSUM_C), A
CCSM_2:                     ; done this value
	INC     BC
	JR      CCSM_1
        
CCSM_4:	CALL	CON_PRT_STR_SP	; running address matches end, done
zoWarnFlow = false
	DEFB    $0d,$0a,"Checksum: ", EOS	; calling it this way preserves HL
zoWarnFlow = true
	LD      A, (CHKSUM_C)
	CALL    CON_PRINTHBYTE      ; checksum overflow first
	CALL    CON_PRINTHWORD
	CALL    CON_PRT_NL

	RET				; returns with checksum in A,HL

;***************************************************************************
; Load hex-intel record
;
;***************************************************************************

HEXI_COMMAND:
        
; :0C 2000 00  C31820C39421C3B62AC3812A 50
;  sz addr typ data                     chk

; This part reads the record into the buffer. 
; Note the ":" is already eaten by the command interpreter.
HEXI_COMMAND:	LD      A, 1
        LD      (MUTE), A
        LD      HL, UPLOADBUF
        LD      (RX_READ_P), HL
        LD      (RX_WRITE_P), HL
HXI_LOOP:	CALL    CON_RX	;SIOA_RX
        LD      (HL), A
        INC     HL
;        AND     A
        CP      LF
        JR      NZ, HXI_LOOP
        
; the record is received, echo the start address
HXI_RCVD:	LD      (RX_WRITE_P), HL
		sub a
        LD      (MUTE), A
        
        LD      HL, UPLOADBUF + 2       ; Point to the first address char.
        LD      B, 4
HXIADRLP:	LD      A, (HL)
        CALL    CON_PRT_CHAR
        INC     HL
        DJNZ    HXIADRLP
        
        
; processing the record
HXI_PROC:	LD      HL, UPLOADBUF
        CALL    CON_CHARS2BYTE              ; get record size
        LD      (ULSIZE), A             ; store it
        CALL    CON_CHARS2BYTE              ; get record address, MSB
        LD      (IECADDR+1), A          ; 
        CALL    CON_CHARS2BYTE              ; get record address, LSB
        LD      (IECADDR), A 
        CALL    CON_CHARS2BYTE              ; get record type
        LD      (IERECTYPE), A
        CP      01h                     ; compare to end record
        JR      Z, HXI_ENDR
        LD      A, (ULSIZE)
        LD      B, A                    ; set up DJNZ loop
        LD      DE, (IECADDR)
HXD_LOOP:	CALL    CON_CHARS2BYTE              ; get data byte
        LD      (DE), A                 ; store it at target location
        INC     DE
        DJNZ    HXD_LOOP                ; repeat for all data bytes

;; unmute
;HXI_UNMT:	LD      A, 0
;        LD      (MUTE), A

        ld a," "
        CALL    CON_PRT_CHAR
        CALL    CON_CHARS2BYTE              ; Get checksum. Not that anyone checks it...
        call CON_PRINTHBYTE
        dec hl
        ld (hl),a			; in case anyone wants to use checksum, store it in last byte of data buffer
;        CALL    CON_PRT_NL

; Done
        xor a	; checksum being returned in A was matching command checks, so zero A
HXI_ENDR:	RET
        
USERRREG:	equ $1fbc
USERAF: EQU     USERRREG+0
USERBC: EQU     USERRREG+2
USERDE: EQU     USERRREG+4
USERHL: EQU     USERRREG+6
UAFP:   EQU     USERRREG+8
UBCP:   EQU     USERRREG+10
UDEP:   EQU     USERRREG+12
UHLP:   EQU     USERRREG+14
USERIX: EQU     USERRREG+16
USERIY: EQU     USERRREG+18
USERSP: EQU     USERRREG+20
USERIF: EQU     USERRREG+22
FLAGH:  EQU     USERRREG+24
FLAGL:  EQU     USERRREG+26
FLAGHP: EQU     USERRREG+28
FLAGLP: EQU     USERRREG+30
USERPC: EQU     USERRREG+32
        
RDLN_1: DEFB    " AF   BC   DE   HL   IX   IY   AF", 027h, "  BC", 027h, "  DE", 027h, "  HL", 027h, EOS
RDLN_3: DEFB    " SP   PC   IF   SZ-H-PNC  SZ-H-PNC", 027h  , EOS

REGDUMP_COMMAND:	LD      HL, RDLN_1
        CALL    CON_PRT_STR
        
        CALL    CON_PRT_NL
        
        LD      HL, (USERAF)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERBC)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERDE)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERHL)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERIX)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERIY)
        CALL    CON_PRINTHWORD
        
        LD      A, " "
        CALL    CON_PRT_CHAR

        LD      HL, (UAFP)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (UBCP)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (UDEP)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (UHLP)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        CALL    CON_PRT_NL
        
        LD      HL, RDLN_3
        CALL    CON_PRT_STR
        
        CALL    CON_PRT_NL
        
        LD      HL, (USERSP)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERPC)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        
        LD      HL, (USERIF)
        CALL    CON_PRINTHWORD
        LD      A, " "
        CALL    CON_PRT_CHAR
        CALL    CON_PRT_CHAR
        
        LD      A, (USERAF+1)
        CALL    PRT8BIT
        LD      A, " "
        CALL    CON_PRT_CHAR
        LD      A, " "
        CALL    CON_PRT_CHAR
        LD      A, (UAFP+1)
        CALL    PRT8BIT
        
        RET
        
REGDMPJ:
        CALL    REGDUMP_COMMAND
        JP      MPFMON  ; return to monitor

; RAM test
; Tssss eeee

TRC_1: DEFB "RAM Test", 0Dh, 0Ah, EOS
TRC_2: DEFB "Location to start in 4 digit HEX: ", EOS
TRC_3: DEFB 0Dh, 0Ah, "Location to end in 4 digit HEX: ", EOS
TRC_4: DEFB 0Dh, 0Ah, "Start address should be before End address", EOS

TRAM_COMMAND:	LD      HL,TRC_1        ;Print some messages 
        CALL    CON_PRT_STR
        LD      HL,TRC_2
        CALL    CON_PRT_STR
        
        CALL    CON_GETHEXWORD              ;HL now points to databyte location	
        LD      (MVADDR), HL
        
        LD      HL,TRC_3
        CALL    CON_PRT_STR
        
        CALL    CON_GETHEXWORD              ;HL now points to databyte location	
        LD      (MVADDR+2), HL
        
        LD      A, (MVADDR+3)   ; End MSB
        LD      HL, MVADDR+1    ; (Start MSB)
        CP      (HL)            ; A - (HL)
        JR      Z, _TC_ZERO     ; When MSBs are on same page, test LSBs
        JR      C, _TC_NEGM      ; When Start MSB > End MSB, report error, exit
_TC_POS:
        CALL    MTEST           ; When End page (MSB) is larger than Start (MSB), go to test
        JR      _TC_DONE
        
_TC_ZERO:        
        LD      A, (MVADDR+2)   ; End LSB
        LD      HL, MVADDR+0    ; (Start LSB)
        CP      (HL)            ; A - (HL)
        JR      C, _TC_NEGL      ; When Start LSB > End LSB, report error, exit
        CALL    MTEST           ; When End page (LSB) is larger than Start (LSB), go to test
        
        JR      _TC_DONE
_TC_NEGM:
_TC_NEGL:
        LD      HL, TRC_4
        CALL    CON_PRT_STR
        JR      _TC_DONE
                
_TC_DONE:        
        RET
        
MTC_1: DEFB 0Dh, 0Ah, " Pass 1: ??h to 00h ", EOS
MTC_2: DEFB 0Dh, 0Ah, " Pass 2: 00h to 55h ", EOS
MTC_3: DEFB 0Dh, 0Ah, " Pass 3: 55h to AAh ", EOS
MTC_4: DEFB 0Dh, 0Ah, " Pass 4: AAh to FFh ", EOS
MTC_5: DEFB 0Dh, 0Ah, " Memory OK", EOS
MTCER1: DEFB " Error at: ", EOS
MTCER2: DEFB " value expected: ", EOS
MTCER3: DEFB ", found: ", EOS

        ; Test strategy in four passes:
        ; 1. Loop through start to end and for each memory location:
        ;    Set to 00h and check new value
        ; 2. Loop through start to end and for each memory location:
        ;    Check old value (00h)
        ;    Set new value 55h
        ;    Check new value
        ; 3. Loop through start to end and for each memory location:
        ;   Check old value (55h)
        ;    Set new value AAh
        ;    Check new value 
        ; 4. Loop through start to end and for each memory location:
        ;   Check old value (AAh)
        ;    Set new value FFh
        ;    Check new value 
        ; Report start of each pass.
        ; Report address of first incorrect value and terminate
        
        ; MVADDR/MVADDR+1 : start address, MVADDR+2/MVADDR+3 : end address
        ; MVADDR+4 : actual value, MVADDR+5 : expected value
        ; D : new value, E : old value
        
MTEST:	LD      IX, MVADDR
; Pass 1   ; check only new value (write phase)
        LD      HL, MTC_1
        CALL    CON_PRT_STR
        
        LD      HL, (MVADDR+0)
        LD      BC, (MVADDR+2)
        LD      E, 64
        LD      D, 000h
        LD      A, 1
        LD      (MTPHFLAG), A
        CALL    MCHECK
        
        JR      C, _MTDONE      ; skip other tests on error
        
; Pass 2   ; check old value and new value (read & write phase)
        LD      HL, MTC_2
        CALL    CON_PRT_STR

        LD      HL, (MVADDR+0)  ; reset start address
        LD      E, 000h         ; old value
        LD      D, 055h         ; new value
        LD      A, 2
        LD      (MTPHFLAG), A
        CALL    MCHECK

        JR      C, _MTDONE      ; skip other tests on error
        
; Pass 3
        LD      HL, MTC_3
        CALL    CON_PRT_STR

        LD      HL, (MVADDR+0)  ; reset start address
        LD      E, 055h         ; old value
        LD      D, 0AAh         ; new value
        LD      A, 3
        LD      (MTPHFLAG), A
        CALL    MCHECK

        JR      C, _MTDONE      ; skip other tests on error
        
; Pass 4
        LD      HL, MTC_4
        CALL    CON_PRT_STR

        LD      HL, (MVADDR+0)  ; reset start address
        LD      E, 0AAh         ; old value
        LD      D, 0FFh         ; new value
        LD      A, 4
        LD      (MTPHFLAG), A
        CALL    MCHECK
        
        JR      C, _MTDONE      ; skip other tests on error
        
        LD      HL, MTC_5       ; Ok text
        CALL    CON_PRT_STR        
_MTDONE:
        RET

MCHECK:
_MCLOOP:
        LD      A, (MTPHFLAG)
        CP      1
        JR      Z, _MCSKIPOLD   ; Skip old value check for pass 1
        ; old value check
        LD      A, E
        LD      (IX+5), A       ; store expected value
        LD      A, (HL)         ; read mem
        LD      (IX+4), A       ; store actual value
        CP      (IX+5)          ; compare with expected
        JR      NZ, _MTLPER1    ; jump to error when unequal
_MCSKIPOLD:
        ; new value write
        LD      A, D
        LD      (HL), A         ; write new value
        LD      (IX+5), A       ; store expected value
        ; new value check
        LD      A, (HL)         ; read new value
        LD      (IX+4), A       ; store actual value
        CP      (IX+5)          ; compare with expected
        JR      NZ, _MTLPER2    ; jump to error when unequal
        CALL    CPADDR          ; 
        INC     HL              ; 
        JR      NZ, _MCLOOP     ; 
        AND     A               ; "Clear Carry flag"
        LD      A, 1
        JR      _MCDONE

; Error handling
_MTLPER1:
        PUSH    AF
        CALL    CON_PRT_NL
        LD      A, "1"
        CALL    CON_PRT_CHAR
        LD      A, "."
        CALL    CON_PRT_CHAR
        POP     AF
        JR      _MTLPER
_MTLPER2:
        PUSH    AF
        CALL    CON_PRT_NL
        LD      A, "2"
        CALL    CON_PRT_CHAR
        LD      A, "."
        CALL    CON_PRT_CHAR
        POP     AF

_MTLPER:
        PUSH    HL              ; keep actual location
        LD      HL, MTCER1      ; at text
        CALL    CON_PRT_STR
        POP     HL
        CALL    CON_PRINTHWORD
        LD      HL, MTCER2      ; expected text
        CALL    CON_PRT_STR
        LD      A, (MVADDR+5)   ; expected value
        CALL    CON_PRINTHBYTE
        LD      HL, MTCER3      ; actual found text
        CALL    CON_PRT_STR
        LD      A, (MVADDR+4)   ; actual value
        CALL    CON_PRINTHBYTE
        CALL    CON_PRT_NL
        SCF                     ; Flag the error for calling routine
_MCDONE:        
        RET

; **********************************************************************
; CPADDR - Compare two addresses, Z-flag set when equal
;  HL contains current address, BC contains end address
;  Z-flag set when equal
; **********************************************************************
CPADDR:	LD      A, B        ; End MSB
        CP      H           ; end MSB - current MSB : B - H
        JR      NZ, _CPDONE ; When MSBs are unequal
        LD      A, C        ; End LSB
        CP      L           ; end LSB - current LSB ; C - L

_CPDONE:
        
        RET
