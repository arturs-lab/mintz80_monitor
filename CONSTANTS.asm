; Constants, extracted to make the versioned file hardware agnostic

ROM_BOTTOM: EQU    00000h       ; Bottom address of ROM

RAM_BOTTOM: EQU    02000H       ; Bottom address of RAM

;IOM-MPF-IP ports:
UART_BASE:  EQU    008h         ; Base port address, P8250A/USART uses 2 ports.
CTC_BASE:   EQU    010H         ; Base port address for Z80 CTC, only CTC2 is used. 64h
SIO		EQU  018h			; SIO port
PIO_BASE:   EQU    01ch         ; Pase port address for Z80 PIO, not used. 68h
;SPEED:      EQU    06Ch         ; DIP-switches for BAUD rate.

aybase:	equ	$00	; myz80 IO board
ymcs:		equ 	$02	; 02 address reg 03 data reg
turbo:	equ 	$d0	; clock divider
beepr:	equ	$d1	; speaker beeper
memmap:	equ 	$d8	; memory map

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
