;***************************************************************************
;  PROGRAM:			Z80 Monitor        
;  PURPOSE:			ROM Monitor Program
;  ASSEMBLER:		original: TASM 3.2 , converted to z80pack/z80asm
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook. Extended MPF-I version: F.J. Kraan
;  CREATE DATE :	05 May 15 / 2022-03-28
;***************************************************************************

ROM_BOTTOM_0000	equ true

            INCLUDE "CONSTANTS.asm" ; copy or edit one of the 
                                  ; CONSTANTS-aaaa-pp.asm files to
                                  ; CONSTANTS.asm
		include "MonitorMain.asm"

endcode	equ $

endprog	equ $

	if def USE_UART
	output_bin "z80monitor_UART.bin",ROM_BOTTOM+$4000,endprog-ROM_BOTTOM
	output_intel "z80monitor_UART.hex",ROM_BOTTOM+$4000,endprog-ROM_BOTTOM
	output_list "z80monitor_UART.lst"
	else
	output_bin "z80monitor_SIO.bin",ROM_BOTTOM+$4000,endprog-ROM_BOTTOM
	output_intel "z80monitor_SIO.hex",ROM_BOTTOM+$4000,endprog-ROM_BOTTOM
	output_list "z80monitor_SIO.lst"
	endif
