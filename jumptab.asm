;***************************************************************************
;JUMPTAB
;Function: Jump table for programs to call monitor routines at a fixed address
;          as their actual addresses change with monitor updates
;***************************************************************************
JUMPTABR:		; jump table in ROM to be copied to JUMPTAB in RAM
jJUMPTAB_INIT	jp JUMPTAB_INIT
jPRINTHNIB		jp PRINTHNIB
jPRINTHBYTE:	jp PRINTHBYTE
jPRINTHWORD	jp PRINTHWORD
jPRINT_CHAR:	jp PRINT_CHAR
jPRINT_STRING:	jp PRINT_STRING
jPRINT_NEW_LINE	jp PRINT_NEW_LINE
jGETHEXNIB		jp GETHEXNIB
jGETHEXBYTE	jp GETHEXBYTE
jGETHEXWORD	jp GETHEXWORD
jGET_CHAR		jp GET_CHAR
jCHARS2BYTE	jp CHARS2BYTE
jUART_RX_RDY	jp UART_RX_RDY
jUART_RX		jp UART_RX
jRX_CHK		jp RX_CHK
jUART_TX_RDY	jp UART_TX_RDY
jUART_TX		jp UART_TX
jUART_PRNT_SP	jp UART_PRNT_SP
jUART_PRNT_STR	jp UART_PRNT_STR
jTO_UPPER		jp TO_UPPER
jCHAR_ISHEX	jp CHAR_ISHEX
jSHFTNIB		jp SHFTNIB
jNIB2CHAR		jp NIB2CHAR
jCHAR2NIB		jp CHAR2NIB
jdel00:		jp del00
jdelay:		jp delay
jchime		jp chime
jbeep:		jp beep
jymzinit		jp ymzinit
jymzwr		jp ymzwr
jymzsetreg		jp ymzsetreg
jepp_prep		jp epp_prep
jepp_upda		jp epp_upda
jepp_prog		jp epp_prog
jCTC_INIT_ALL	jp CTC_INIT_ALL
jCTC0_INIT		jp CTC0_INIT
jCTC1_INIT		jp CTC0_INIT
jCTC2_INIT		jp CTC0_INIT
jCTC3_INIT		jp CTC0_INIT
jCTC_TC_INIT	jp CTC_TC_INIT
jSIO_INIT		jp SIO_INIT
jSIOA_INIT		jp SIOA_INIT
jSIOB_INIT		jp SIOB_INIT
jSIO_A_INT_SET	jp SIO_A_INT_SET
jSIOA_RTS_OFF	jp SIOA_RTS_OFF
jSIOA_RTS_ON	jp SIOA_RTS_ON
jSIO_A_DI		jp SIO_A_DI
jSIO_A_EI		jp SIO_A_EI
jSIOA_PRNT_SP	jp SIOA_PRNT_SP
jSIOA_PRNT_STR	jp SIOA_PRNT_STR
jSIOA_TX_RDY	jp SIOA_TX_RDY
jSIOA_TX		jp SIOA_TX
jSIOA_RX_WAIT	jp SIOA_RX_WAIT
jSIOA_RX_CHK	jp SIOA_RX_CHK
jSIOA_RX		jp SIOA_RX
jPIO_INIT		jp PIO_INIT
jCF_INIT:		jp CF_INIT
jCF_LP_BUSY:	jp CF_LP_BUSY
jCF_LP_CMD_RDY:	jp CF_LP_CMD_RDY
jCF_LP_DAT_RDY:	jp CF_LP_DAT_RDY
jCF_RD_CMD:	jp CF_RD_CMD
jCF_RD_SECT:	jp CF_RD_SECT
jCF_WR_CMD:	jp CF_WR_CMD
jj:	jp 0
epp_toggle:	in a,(memmap)	; call this at $ffnn after executing epp_prep
		xor a,$02
		out (memmap),a
		jp $0


JUMPTAB_END:	equ $
