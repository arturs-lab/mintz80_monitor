;***************************************************************************
;  PROGRAM:			Z80 Monitor        
;  PURPOSE:			ROM Monitor Program
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook. Extended MPF-I version: F.J. Kraan
;  CREATE DATE :	05 May 15 / 2022-03-28
;***************************************************************************

ROM_BOTTOM_a000	equ true

            INCLUDE "CONSTANTS.asm" ; copy or edit one of the 
                                  ; CONSTANTS-aaaa-pp.asm files to
                                  ; CONSTANTS.asm
		include "MonitorMain.asm"

endcode	equ $

	if def USE_UART
		org $BFE0
	db "z80monitor_a000_UART_0A867B",0

		org $BFFD
	db $0a,$86,$7b
	else
		org $BFE0
	db "z80monitor_a000_SIO_0A839D",0

		org $BFFD
	db $0a,$83,$9d
	endif

endprog	equ $

	if def USE_UART
	output_bin "z80monitor_a000_UART.bin",ROM_BOTTOM,endprog-ROM_BOTTOM	; 
	output_intel "z80monitor_a000_UART.hex",ROM_BOTTOM,endprog-ROM_BOTTOM	;
	output_list "z80monitor_a000_UART.lst"
	else
	output_bin "z80monitor_a000_SIO.bin",ROM_BOTTOM,endprog-ROM_BOTTOM	; 
	output_intel "z80monitor_a000_SIO.hex",ROM_BOTTOM,endprog-ROM_BOTTOM	;
	output_list "z80monitor_a000_SIO.lst"
	endif