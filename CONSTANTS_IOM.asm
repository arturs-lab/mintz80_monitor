; Constants, extracted to make the versioned file hardware agnostic

ROM_BOTTOM: EQU    00000h       ; Bottom address of ROM

RAM_BOTTOM: EQU    04000H       ; Bottom address of RAM

;IOM-MPF-IP ports:
UART_BASE:  EQU    008h         ; Base port address, P8251A/USART uses 2 ports.
CTC_BASE:   EQU    064H         ; Base port address for Z80 CTC, only CTC2 is used.
PIO_BASE:   EQU    068h         ; Pase port address for Z80 PIO, not used.
;SPEED:      EQU    06Ch         ; DIP-switches for BAUD rate.

aybase:	equ	$00	; myz80 IO board
;ymcs:	equ 	$02	; f6 address reg f7 data reg
turbo:	equ 	$d0	; clock divider
beepr:	equ	$d1	; speaker beeper
memmap:	equ 	$d8	; memory map

;$0000-$1fff ROM
;$2000-$3fff RAM
;$4000-$5fff RAM
;$6000-$7fff RAM
;$8000-$9fff RAM
;$a000-$bfff RAM
;$c000-$dfff RAM
;$e000-$ffff RAM

