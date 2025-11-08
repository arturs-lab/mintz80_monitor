;***************************************************************************
;  PROGRAM:			Z80 Monitor        
;  PURPOSE:			ROM Monitor Program
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook. Extended MPF-I version: F.J. Kraan
;  CREATE DATE :	05 May 15 / 2022-03-28
;***************************************************************************

            ORG ROM_BOTTOM

ROUTINES:
R_MAIN:     JP      MAIN            ; init DART and starts command loop
R_U_INIT:   JP      SIO_INIT       ; configures DARTchannel B 
R_PRT_NL:   JP      CON_PRT_NL  ; sends a CR LF
R_PRT_STR:  JP      CON_PRT_STR    ; sends a NULL terminated string
            DEFS    3   ; spare  entries
            DEFS    3
            DEFS    3
            DEFS    3
            
            ORG ROM_BOTTOM + 24     ; room for eight routine entries

            ORG ROM_BOTTOM + $38     ; IRQ routine

IRQ:		di
		ex af,af'
		exx
		jp IRQTAB+(IRQV-IRQTABR)

irq_end:	ex af,af'
		exx
		ei
irq_exit:	reti

            ORG ROM_BOTTOM + $66     ; nmi routine

NMI:		jp IRQTAB+(NMIV-IRQTABR)
nmi_end:	retn

code_end:	dw endcode				; store address of last byte of this code in a defined place for checksum calculation

; we want all drivers at the beginning so they stay the same even if we remove monitor code
		include "EEPROMProg.asm";
		include "CTCDriver.asm"
		include "SIODriver.asm"
		include "PIODriver.asm"
		include "YMZDrvr.asm";

		INCLUDE "CFDriver.asm"
	if def USE_UART
		INCLUDE	"CONIO.asm"		; use UART for console
	else
		INCLUDE	"CONIO_SIO.asm"		; use SIO for console
	endif

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
; at CPU CLK = 10MHz:
; $01 - 52us -> 19.23kHz

del00:	ld a,$00		; delay loop
delay:	push af			; count delay
del1:	dec a
	jr nz,del1		; repeat till 0
	pop af
	dec a
	jr nz,delay
	ret

chime:	ld bc,$0200	; bc = duration
	ld a,$0c		; a = pitch
	; fall through to beep

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

;		include "irqtab.asm"		; table for interrupts
		include "irqtab.asm"

; https://jgmalcolm.com/z80/advanced/im2i
;
IRQTAB_INIT:	push HL
			push DE
			push BC

			ld hl,irq_exit
			ld (IRQTAB),hl		; initialize vector to interrupt return

			ld hl,IRQTAB
			ld de,IRQTAB+2
			ld bc,256/2
			ldir				; fill vector table with vectors pointing to interrupt exit

			ld hl,IRQTABR
			ld de,IRQTAB
			ld bc,IRQTABEND-IRQTABR

			jr TAB_INIT

; Coopy jump table from EEPROM to RAM so that routines can be swapped out
JUMPTAB_INIT:	push HL
			push DE
			push BC

			ld hl,JUMPTABR
			ld de,JUMPTAB
			ld bc,JUMPTAB_END-JUMPTABR

TAB_INIT:		ldir

			pop BC
			pop DE
			pop HL
			ret

		include "jumptab.asm"		; bios and monitor routine jump table

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

	ld sp,SP_INIT

	ld a,$01
	out ($f4),a	; set INTPR register, SIO-CTC-PIO priority. section 3.9, page 149

;	call memmap_init

	ld a,$20
	call delay		; looks like Z80 needs this delay to successfully write to IO ports
	ld a,CLKDIV		; (SYSCLK MHz/2/(value+1))
	out (turbo),a
	call chime
	call ymzinit
	call IRQTAB_INIT	; copy interrupt jump table from (IRQTABR) in eeprom to (IRQTAB) in ram
	call JUMPTAB_INIT	; copy jump table from (JUMPTABR) in EEPROM to (JUMPTAB) in RAM
	call PIO_INIT		; init PIO
	call CTC_INIT_ALL     ; init CTC
	call SIO_INIT         ; init SIO, CTC drives SIO, so has to be set first
	CALL UART_INIT		; Initialize UART
	call CON_PRT_STR_SP	; print banner
zoWarnFlow = false
	db $0d,$0a,"Hellorld",$0d,$0a,"CPLD config:",0
zoWarnFlow = true

	call dmpio			; dump CPLD configuration

	call CON_PRT_STR_SP	; print banner
zoWarnFlow = false
	db "System clock ",SYSCLK,"MHz",$0d,$0a,0
zoWarnFlow = true

	ld hl,R_MAIN		; Calculate my own checksum
	ld (MVADDR+0),hl
	ld hl,(code_end)
	ld (MVADDR+2),hl
	call CCKSM_DO

	call CF_BOOT		; try to boot from CF

gomain:	CALL PRINT_MON_HDR	;Print the monitor header info
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
	CALL CON_PRT_STR
	RET
			
;***************************************************************************
;RESET_COMMAND
;Function: Software Reset to $0000
;***************************************************************************
RESET_COMMAND:
	JP R_MAIN		;Jumps to 0000 (MPF-1 monitor re-entry)	
			
;***************************************************************************
;PRINT_MON_HDR
;Function: Print out program header info
;***************************************************************************
MNMSG1:     DEFB    0DH, 0Ah, "MintZ80 Computer", 09h, 09h, 09h, "2015 MCook"
MNMSG2:     DEFB    0DH, 0Ah, " adaptation to MPF-1 / Z80 DART", 09h, "2022 F.J.Kraan", 0Dh, 0Ah
            DEFB    "Adaptation to MintZ80 2025 Artur's Lab", 0Dh, 0Ah
MNMSG3A:    DEFB    "Monitor v", VERSMYR, ".", VERSMIN, ", ROM: ", EOS
MNMSG3B:    DEFB    "h, RAM: ", EOS
MNMSG3C:    DEFB    "h, CTC: ", EOS
MNMSG3D:    DEFB    "h, SIO: ", EOS
MNMSG3E:    DEFB    "h, PIO: ", EOS
MNMSG3F:    DEFB    "h", 0Dh, 0AH, 0Dh, 0AH
MONHLP:     DEFB    09h," Input ? for command list", 0Dh, 0AH, EOS
MONERR:     DEFB    0Dh, 0AH, "Error in params: ", EOS

PRINT_MON_HDR:
        CALL    CLEAR_SCREEN        ;Clear the terminal screen
        LD      HL, MNMSG1          ;Print some messages
        CALL    CON_PRT_STR
        LD      HL, ROM_BOTTOM
        CALL    CON_PRINTHWORD
        LD      HL, MNMSG3B         ; 2nd part, RAM
        CALL    CON_PRT_STR
        LD      HL, RAM_BOTTOM
        CALL    CON_PRINTHWORD
        LD      HL, MNMSG3C         ; 3rd part CTC
        CALL    CON_PRT_STR
        LD      A, CTC_BASE
        CALL    CON_PRINTHBYTE
        LD      HL, MNMSG3D         ; 4th part SIO
        CALL    CON_PRT_STR
        LD      A, SIO_BASE
        CALL    CON_PRINTHBYTE
        LD      HL, MNMSG3E         ; 5th part PIO
        CALL    CON_PRT_STR
        LD      A, PIO_BASE
        CALL    CON_PRINTHBYTE
        LD      HL, MNMSG3F         ; 6th part, line ending
        CALL    CON_PRT_STR
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
        CALL    CON_PRT_STR
        CALL    CON_GET_CHAR        ; Get a character from user into Acc
        CALL    CON_PRT_CHAR
        CP      CR
        JR      Z, _MPL_CR
        CALL    CON_PRT_NL  ; Print a new line
_MPL_CR:
        CALL    MON_COMMAND     ; Respond to user input
        CALL    CON_PRT_NL  ; Print a new line
        JR      MON_PRMPT_LOOP

;***************************************************************************
;MON_COMMAND
;Function: User input in accumulator to respond to 
;***************************************************************************
MON_COMMAND:    ; Inserted ERROR_CHK for all commands requiring input
        CALL    CLEAR_ERROR
        CP      "?"
        CALL    Z,HELP_COMMAND
        CP      "A"
        CALL    Z,hex_compare
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
        
ERROR_CHK:	LD      A, (ERRFLAG)
        CP      E_NONE
        RET     Z
        LD      HL, MONERR
        CALL    CON_PRT_STR
        LD      A, (ERRFLAG)
        CALL    CON_PRINTHBYTE
        CALL    CON_PRT_NL
CLEAR_ERROR:	PUSH    AF
        LD      A, E_NONE
        LD      (ERRFLAG), A
        POP     AF
        RET
        
        INCLUDE	"MONCommands.asm"

;		jmp 0
MON_CLS: DEFB 0Ch, EOS  				;Escape sequence for CLS. (aka form feed) 
		

;        END

dmpio:	ld c,$d0
		ld b,$0
		CALL	CON_PRT_NL
dmpio1:	IN      A, (C)
		CALL	CON_PRINTHBYTE
		inc c
		ld a,c
		cp $e0
		jr nz,dmpio1
		CALL	CON_PRT_NL
		ret

zero_ram:	ld hl,zero_ram_r
		ld de,RAM_BOTTOM
		ld bc, zero_ram_end - zero_ram_r
		ldir
		jp RAM_BOTTOM
zero_ram_r:	ld hl,RAM_BOTTOM + (zero_ram_end - zero_ram_r)
		ld de,hl
		inc de
		ld bc,$ffff - RAM_BOTTOM + (zero_ram_end - zero_ram_r)
		xor a
		ld (hl),a
		ldir
		jp 0
zero_ram_end:	equ $

jphl:		jp (hl)

;		INCLUDE	"DARTDriver.asm"
;		INCLUDE	"UARTDriver.asm"

