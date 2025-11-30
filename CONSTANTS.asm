VERSMYR:    EQU     "1"
VERSMIN:    EQU     "3"

; clock divider
CLKDIV	EQU	0	; (SYSCLK MHz/2/(value+1))
SYSCLK	EQU "9.216"
MACHINE	EQU "MintZ80"

; Constants, extracted to make the versioned file hardware agnostic

; we could use this to trigger including UART code
; but I want flexibility to include it but not use for console.
; USE_UART:	EQU true

; do wew want to enable interrupts?
EN_INT:	EQU true

; ### MEM map
RAM_BOTTOM:	EQU 02000H       ; Bottom address of RAM
RAM_TOP:	EQU $ffff


	if def ROM_BOTTOM_c000

ROM_BOTTOM:	EQU $c000       ; Bottom address of ROM
ROM_TOP:	EQU ROM_BOTTOM + 01FFFh		; Top address of ROM

JUMPTAB:	EQU RAM_TOP - $2FF	; jump table for monitor routines
IRQTAB:	EQU RAM_TOP - $3FF	; interrupt vector table
SP_INIT:	EQU 0				; initial value of SP
CFSECT_BUF_V:	EQU $A000		; value for CFSECT_BUF variable. Defaults to $c000 in preparation for CPM loader
MONVARS:	EQU RAM_TOP - $1ff	; SP goes at the top of memory. Put monitor vars and buffers 511 bytes below it
epp_tmp:	EQU RAM_TOP - $ff	; this is where EEPROM programming code is copied before execution to avoid it
						; clashing with new data being programmed into its location in EEPROM
USERRREG:	equ RAM_TOP - $37F
	elseif def ROM_BOTTOM_a000

ROM_BOTTOM:	EQU $a000       ; Bottom address of ROM
ROM_TOP:	EQU ROM_BOTTOM + 01FFFh		; Top address of ROM

JUMPTAB:	EQU $c000 - $300	; jump table for monitor routines
IRQTAB:	EQU $c000 - $400	; interrupt vector table
SP_INIT:	EQU $c000			; initial value of SP
CFSECT_BUF_V:	EQU $C000		; value for CFSECT_BUF variable. Defaults to $c000 in preparation for CPM loader
MONVARS:	EQU $c000 - $200	; SP goes at the top of memory. Put monitor vars and buffers 511 bytes below it
epp_tmp:	EQU $c000 - $100	; this is where EEPROM programming code is copied before execution to avoid it
						; clashing with new data being programmed into its location in EEPROM
USERRREG:	equ $c000 - $380
	else

ROM_BOTTOM:	EQU 00000h       ; Bottom address of ROM
ROM_TOP:	EQU ROM_BOTTOM + 01FFFh		; Top address of ROM

JUMPTAB:	EQU RAM_BOTTOM + $0000	; jump table for monitor routines
IRQTAB:	EQU RAM_BOTTOM + $0100	; interrupt vector table
SP_INIT:	EQU RAM_BOTTOM + $0400	; initial value of SP
CFSECT_BUF_V:	EQU $C000			; value for CFSECT_BUF variable. Defaults to $c000 in preparation for CPM loader
MONVARS:	EQU RAM_BOTTOM + $0200	; SP goes at the top of memory. Put monitor vars and buffers 511 bytes below it
epp_tmp:	EQU RAM_BOTTOM + $0300	; this is where EEPROM programming code is copied before execution to
							; avoid it clashing with new data being programmed into its location in EEPROM
USERRREG:	equ RAM_BOTTOM + $0180
	endif


; these interrupt bases are added to Z80 interrupt vector register I to form final vector in IM2
SIOV:		EQU $0		; SIO interrupt vector base except bits 2-0 are set according to interrupt type, 16 interrupts
CTCV:		EQU $10		; CTC interrupt vector base, 4 interrupts
PIOV:		EQU $18		; PIO interrupt vector base, 2 interrupts

;MPFMON:	EQU 0000h
UPLOADBUF:	EQU MONVARS + 0h     ; Buffer for hex-intel upload. Allows up to 128 bytes (80h) per line.
ULBUFSIZE:	EQU 80h                  ; a 20h byte hex-intel record use 75 bytes...
ULBEND:	EQU UPLOADBUF + ULBUFSIZE
MSGBUF:	EQU UPLOADBUF
RX_READ_P:	EQU MONVARS + $80     ; read pointer
RX_WRITE_P:	EQU MONVARS + $82     ; write pointer
ASCDMPBUF:	EQU MONVARS + $84      ;Buffer to construct ASCII part of memory dump
ASCDMPEND:	EQU MONVARS + $94     ;End of buffer, fill with EOS
DMPADDR:	EQU MONVARS + $95     ;Last dump address
MVADDR:	EQU MONVARS + $96     ; 6 bytes: start-address, end-address, dest-address or fill-value (23, 24, 25, 26, 27, 28)
ERRFLAG:	EQU MONVARS + $9c     ; Location to store 
MUTE:		EQU MONVARS + $9d     ; 0 - print received chars, 1 - do not print received chars
ULSIZE:	EQU MONVARS + $9e     ; actual size of current/last hex-intel message
IECHECKSUM:	EQU MONVARS + $9f        ; hex-intel record checksum
IECADDR:	EQU MONVARS + $a0        ; hex-intel record address (2 bytes)
IERECTYPE:	EQU MONVARS + $a2        ; hex-intel record type
DEBUG:	EQU MONVARS + $a3
MTPHFLAG:	EQU MONVARS + $a3     ; Phase counter: phase 1 doesn't check old value (being unknown)
CHKSUM_C:	EQU MONVARS + $a4     ; uses 3 bytes

CF_SECCNT:	EQU MONVARS + $a7 
CF_LBA0:	EQU MONVARS + $a8
CF_LBA1:	EQU MONVARS + $a9
CF_LBA2:	EQU MONVARS + $aa
CF_LBA3:	EQU MONVARS + $ab
CF_PART_CUR:	EQU MONVARS + $ac	; Current partition offset into MBR
CFSECT_BUF:	EQU MONVARS + $ae	; pointer to location of CF data buffer. Need $200 byte buffer

SYSTMR0:	EQU MONVARS + $b0		; system timers. SYSTMR0 overflowing into SYSTMR2 gets incremented on T0 interrupt every 1ms
SYSTMR2:	EQU MONVARS + $b2
SYSTMR4:	EQU MONVARS + $b4		; SYSTMR4 overflowing into SYSTMR6 gets incremented on T1 interrupt, 5ms
SYSTMR5:	EQU MONVARS + $b5		; seconds
SYSTMR6:	EQU MONVARS + $b6		; minutes
SYSTMR7:	EQU MONVARS + $b7		; hours
SYSTMR8:	EQU MONVARS + $b8		; days, 16 bit
SYSTMR9:	EQU MONVARS + $b9

; SIO
SIOA_WR0:		EQU MONVARS + $e7
SIOA_WR1:		EQU MONVARS + $e8
SIOA_WR3:		EQU MONVARS + $e9
SIOA_WR4:		EQU MONVARS + $ea
SIOA_WR5:		EQU MONVARS + $eb
SIOA_WR6:		EQU MONVARS + $ec
SIOA_WR7:		EQU MONVARS + $ed
SIOB_WR0:		EQU MONVARS + $ee
SIOB_WR1:		EQU MONVARS + $ef
SIOB_WR2:		EQU MONVARS + $f0
SIOB_WR3:		EQU MONVARS + $f1
SIOB_WR4:		EQU MONVARS + $f2
SIOB_WR5:		EQU MONVARS + $f3
SIOB_WR6:		EQU MONVARS + $f4
SIOB_WR7:		EQU MONVARS + $f5
; PIO
PIO_CH0_CNF:	EQU MONVARS + $f6
PIO_CH1_CNF:	EQU MONVARS + $f7
; CTC prescaler value locations
CTC_CH0_CNF:	EQU MONVARS + $f8
CTC_CH1_CNF:	EQU MONVARS + $f9
CTC_CH2_CNF:	EQU MONVARS + $fa
CTC_CH3_CNF:	EQU MONVARS + $fb
; location of time constant values for CTC channels
CTC_CH0_TC:	EQU MONVARS + $fc		; time constant for channel 0 system interrupt 200Hz
CTC_CH1_TC:	EQU MONVARS + $fd		; time constant for channel 1
CTC_CH2_TC:	EQU MONVARS + $fe		; time constant for channel 2 this feeds SIOB
CTC_CH3_TC:	EQU MONVARS + $ff		; time constant for channel 3 this feeds SIOA


; ### IO map
;IOM-MPF-IP ports:
UART_BASE:	EQU 008h         ; Base port address, P8250A/USART uses 8 ports.
CTC_BASE:	EQU 010H         ; Base port address for Z80 CTC, only CTC2 is used. 64h
CTC_CH0:	EQU CTC_BASE		; system interrupt 200Hz
CTC_CH1:	EQU CTC_BASE+1
CTC_CH2:	EQU CTC_BASE+2		; this feeds SIOB
CTC_CH3:	EQU CTC_BASE+3		; this feeds SIOA
SIO_BASE:	EQU 018h			; SIO port
SIO_DA:	EQU SIO_BASE
SIO_CA:	EQU SIO_BASE+1
SIO_DB:	EQU SIO_BASE+2
SIO_CB:	EQU SIO_BASE+3
PIO_BASE:	EQU 01ch         ; Base port address for Z80 PIO, not used. 68h
PIO_DA:	EQU PIO_BASE
PIO_CA:	EQU PIO_BASE+1
PIO_DB:	EQU PIO_BASE+2
PIO_CB:	EQU PIO_BASE+3

CNFIGSW	EQU $a0	; config switch read only
ymbase:	EQU $b0	; 02 address reg 03 data reg, on mint board $70/$b0

CF_RESET	EQU 0B8h	; CF soft reset write only if configured by jumper
CFBASE:	EQU 080h
;The addresses that the CF Card resides in I/O space.
;Change to suit hardware.
CFDATA:	EQU CFBASE + 00h		; Data (R/W)
CFERR:	EQU CFBASE + 01h		; Error register (R)
CFFEAT:	EQU CFBASE + 01h		; Features (W)
CFSECCO:	EQU CFBASE + 02h		; Sector count (R/W)
CFLBA0:	EQU CFBASE + 03h		; LBA bits 0-7 (R/W, LBA mode)
CFLBA1:	EQU CFBASE + 04h		; LBA bits 8-15 (R/W, LBA mode)
CFLBA2:	EQU CFBASE + 05h		; LBA bits 16-23 (R/W, LBA mode)
CFLBA3:	EQU CFBASE + 06h		; LBA bits 24-27 (R/W, LBA mode)
CFSTAT:	EQU CFBASE + 07h		; Status (R)
CFCMD:	EQU CFBASE + 07h		; Command (W)
CFCTL:	EQU CFBASE + 08h + 06h	; write: Device control
CFALTSTAT:	EQU CFBASE + 08h + 06h	; read: Alternate status
CFADDR:	EQU CFBASE + 08h + 07h	; read: Drive address

turbo:	EQU $d0	; clock divider 0=4MHz, 1=2MHz, 2=1.33MHz, 3=1MHz
beepr:	EQU $d1	; speaker beeper
memmap:	EQU $d8	; memory map $d8-$df

; ### other
; SIO config values
SIOA_WR0_CV:		EQU 00110000b	; write into WR0: error reset
SIOA_WR1_CV:		EQU 00000000b	; no interrupts
SIOA_WR3_CV:		EQU 00001100b	; write into WR3: RX disable;
SIOA_WR4_CV:		EQU 01000100b	; write into WR4: presc. 16x, 1 stop bit, no parity
SIOA_WR5_CV:		EQU 11101000b	; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
SIOA_WR6_CV:		EQU 0
SIOA_WR7_CV:		EQU 0
SIOB_WR0_CV:		EQU 0
SIOB_WR1_CV:		EQU 00011100b	; RX int enable, parity does not affect vector, status affects vector
SIOB_WR2_CV:		EQU SIOV		; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
							; will be affected by the channel & condition that raised the interrupt
							; (see datasheet): in our example, 0x0C for Ch.A receiving a char, 0x0E
							; for special conditions
SIOB_WR3_CV:		EQU 0
SIOB_WR4_CV:		EQU 0
SIOB_WR5_CV:		EQU 0
SIOB_WR6_CV:		EQU 0
SIOB_WR7_CV:		EQU 0

; PIO config values
PIO_CH0_CNFV:	EQU 11001111b
PIO_CH1_CNFV:	EQU 11001111b
; CTC config values
CTC_CH0_CNFV:	EQU 10100111b
CTC_CH1_CNFV:	EQU 10100111b
CTC_CH2_CNFV:	EQU 01110111b
CTC_CH3_CNFV:	EQU 01110111b
; CTC time constants values
CTC_CH0_TV:	EQU $24	; $24 -> 1000Hz, 1ms, $12 -> 2000Hz, 500us
CTC_CH1_TV:	EQU $b4	; 180=$b4 system interrupt, $b4 -> 200Hz, 5ms
CTC_CH2_TV:	EQU $0f	; SIOB 9600 baud with 16x prescaler in SIO ; @4MHz CPU: 11=57600baud, 1a=38400baud, 34=19200baud, 45=14400baud, 68=9600baud, d0=4800baud
CTC_CH3_TV:	EQU $0f	; SIOA 9600 baud with 16x prescaler in SIO ; @4.608MHz CPU: 14=115200, 28=57600, 3c=38400, 78=19200, a0=14400, f0=9600baud
					; with 256x prescaler in SIO ; @4.608MHz CPU: 01=14400, 0f=9600, 1e=4800, 3c=2400
					; with 256x prescaler in SIO ; @9.216MHz CPU: 0f=19200, 14=14400, 1e=9600, 3c=4800, 78=2400

; Error codes intel Hex record
E_NONE:	EQU 00h
E_NOHEX:	EQU 01h			; input char not 0-9, A-F
E_PARAM:	EQU 02h			; inconsistent range; start > end
E_BUFSIZE:	EQU 03h			; size larger than buffer
E_HITYP:	EQU 04h			; unsupported hex-intel record type
E_HICKSM:	EQU 05h			; hex-intel record checksum error
E_HIEND:	EQU 06h			; hex-intel end record type found

HI_DATA:	EQU 00h
HI_END:	EQU 01h

ESC:		EQU 01Bh		; 
EOS:		EQU 000h		; End of string
MUTEON:	EQU 001h
LF:		EQU 00Ah
CR:		EQU 00Dh


epp_src:	EQU $4000	; source of code to be programmed into EEPROM
epp_tgt:	EQU $0000	; target starting address
epp_len:	EQU $2000	; byte count of data to be programmed
epp_del:	EQU $1b	; delay between EEPROM readbacks, about 10ms max per datasheet 
epp_bank:	EQU $01	; eeprom bank to select. by default program RAM bank to allow testing and reset if programming fails

; for hex dump routine: number of lines to print
HEXLINES:	EQU 17 ; FIXIT: There is a off-by-one-error here


;$0000-$1fff ROM	d8 00->rom0, 02->rom2
;$2000-$3fff RAM	d9 00->rom1, 02->rom3
;$4000-$5fff RAM	da 00->rom0, 02->rom2
;$6000-$7fff RAM	db 00->rom1, 02->rom3
;$8000-$9fff RAM	dc 00->rom0, 02->rom2
;$a000-$bfff RAM	dd 00->rom1, 02->rom3
;$c000-$dfff RAM	de 00->rom0, 02->rom2
;$e000-$ffff RAM	df 00->rom1, 02->rom3

; rom0-rom3 refer to 8K chunks of 28C256 32K EEPROM