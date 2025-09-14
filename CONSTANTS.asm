; Constants, extracted to make the versioned file hardware agnostic

; ### MEM map
ROM_BOTTOM: EQU    00000h       ; Bottom address of ROM
ROM_TOP:     EQU    ROM_BOTTOM + 01FFFh		; Top address of ROM

RAM_BOTTOM: EQU    02000H       ; Bottom address of RAM
RAM_TOP:     EQU    $ffff

JUMPTAB:	EQU	RAM_TOP - $2FF

CFSECT_BUFF:EQU     RAM_TOP - $0fff ; $3FF
CFSECT_END:	EQU		CFSECT_BUFF + 0200h

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

; ### IO map
;IOM-MPF-IP ports:
UART_BASE:  EQU    008h         ; Base port address, P8250A/USART uses 2 ports.
CTC_BASE:   EQU    010H         ; Base port address for Z80 CTC, only CTC2 is used. 64h
CTC_CH0:	equ CTC_BASE
CTC_CH1:	equ CTC_BASE+1
CTC_CH2:	equ CTC_BASE+2
CTC_CH3:	equ CTC_BASE+3
SIO_BASE:	EQU  018h			; SIO port
SIO_DA      equ SIO_BASE
SIO_CA      equ SIO_BASE+1
SIO_DB      equ SIO_BASE+2
SIO_CB      equ SIO_BASE+3
PIO_BASE:   EQU    01ch         ; Pase port address for Z80 PIO, not used. 68h
PIO_DA      equ $1c
PIO_DB      equ $1e
PIO_CA      equ $1d
PIO_CB      equ $1f

;SPEED:      EQU    06Ch         ; DIP-switches for BAUD rate.

; location of time constant values for CTC channels
CTC_CH0_TC	equ	$fefc		; time constant for channel 0
CTC_CH1_TC	equ	$fefd		; time constant for channel 1
CTC_CH2_TC	equ	$fefe		; time constant for channel 2
CTC_CH3_TC	equ	$feff		; time constant for channel 3

ymbase:	equ	$02

;aybase:	equ	$00	; myz80 IO board
;ymcs:		equ 	$02	; 02 address reg 03 data reg

CFBASE:		EQU		090h
;The addresses that the CF Card resides in I/O space.
;Change to suit hardware.
CFDATA:		EQU	CFBASE + 00h		; Data (R/W)
CFERR:		EQU	CFBASE + 01h		; Error register (R)
CFFEAT:		EQU	CFBASE + 01h		; Features (W)
CFSECCO:	EQU	CFBASE + 02h		; Sector count (R/W)
CFLBA0:		EQU	CFBASE + 03h		; LBA bits 0-7 (R/W, LBA mode)
CFLBA1:		EQU	CFBASE + 04h		; LBA bits 8-15 (R/W, LBA mode)
CFLBA2:		EQU	CFBASE + 05h		; LBA bits 16-23 (R/W, LBA mode)
CFLBA3:		EQU	CFBASE + 06h		; LBA bits 24-27 (R/W, LBA mode)
CFSTAT:		EQU	CFBASE + 07h		; Status (R)
CFCMD:		EQU	CFBASE + 07h		; Command (W)

turbo:	equ 	$d0	; clock divider
beepr:	equ	$d1	; speaker beeper
memmap:	equ 	$d8	; memory map

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


epp_src:	equ $2000	; source of code to be programmed into EEPROM
epp_tgt:	equ $0000	; target starting address
epp_len:	equ $2000	; byte count of data to be programmed
epp_del:	equ $1b	; delay between EEPROM readbacks, about 10ms max per datasheet 
epp_tmp:	equ $ff00	; this is where EEPROM programming code is copied before execution to avoid it clashing with new data being programmed into its location in EEPROM
epp_bank:	equ $02	; eeprom bank to select. by default program shadow bank to allow testing and reset if programming fails

;$0000-$1fff ROM
;$2000-$3fff RAM
;$4000-$5fff RAM
;$6000-$7fff RAM
;$8000-$9fff RAM
;$a000-$bfff RAM
;$c000-$dfff RAM
;$e000-$ffff RAM
