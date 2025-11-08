;***************************************************************************
;JUMPTAB
;Function: Jump table for programs to call monitor routines at a fixed address
;          as their actual addresses change with monitor updates
;***************************************************************************
JUMPTABR:		; jump table in ROM to be copied to JUMPTAB in RAM
jepp_switch	jp epp_switch
jCON_RX_CHK	jp CON_RX_CHK
jCON_RX		jp CON_RX
jCON_TX		jp CON_TX
jCON_PRT_STR_SP	jp CON_PRT_STR_SP
jCON_PRT_STR:	jp CON_PRT_STR
jCON_PRT_CHAR:	jp CON_PRT_CHAR
jCON_PRT_NL:	jp CON_PRT_NL
jCON_GET_CHAR	jp CON_GET_CHAR
jCON_CHAR_ISHEX	jp CON_CHAR_ISHEX
jCON_GETHEXNIB	jp CON_GETHEXNIB
jCON_GETHEXBYTE	jp CON_GETHEXBYTE
jCON_GETHEXWORD	jp CON_GETHEXWORD
jCON_NIB2CHAR	jp CON_NIB2CHAR
jCON_SHFTNIB	jp CON_SHFTNIB
jCON_PRINTHNIB:	jp CON_PRINTHNIB
jCON_PRINTHBYTE:	jp CON_PRINTHBYTE
jCON_PRINTHWORD:	jp CON_PRINTHWORD
jCON_CHAR2NIB	jp CON_CHAR2NIB
jCON_CHARS2BYTE	jp CON_CHARS2BYTE
jCON_TO_UPPER		jp CON_TO_UPPER
jdel00:		jp del00
jdelay:		jp delay
jchime		jp chime
jbeep:		jp beep
jymzinit		jp ymzinit
jymzwr		jp ymzwr
jymzsetreg		jp ymzsetreg
jepp_prep		jp epp_prep
jepp_upda		jp epp_upda
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
jSIOB_PRNT_SP	jp SIOB_PRNT_SP
jSIOB_PRNT_STR	jp SIOB_PRNT_STR
jSIOB_TX_RDY	jp SIOB_TX_RDY
jSIOB_TX		jp SIOB_TX
jSIOB_RX_WAIT	jp SIOB_RX_WAIT
jSIOB_RX_CHK	jp SIOB_RX_CHK
jSIOB_RX		jp SIOB_RX
jPIO_INIT		jp PIO_INIT
jCF_INIT:		jp CF_INIT
jCF_LP_BUSY:	jp CF_LP_BUSY
jCF_LP_CMD_RDY:	jp CF_LP_CMD_RDY
jCF_LP_DAT_RDY:	jp CF_LP_DAT_RDY
jCF_RD_CMD:	jp CF_RD_CMD
jCF_RD_SECT:	jp CF_RD_SECT
jCF_WR_CMD:	jp CF_WR_CMD
jCF_WR_SECT:	jp CF_WR_SECT
jCF_SETUP_LBA:	jp CF_SETUP_LBA
jCF_SETUP_PART:	jp CF_SETUP_PART
jCF_PART_NEXT:	jp CF_PART_NEXT
jCF_SYSLD:		jp CF_SYSLD
jCCKSM_DO:		jp CCKSM_DO
jJUMPTAB_INIT:	jp JUMPTAB_INIT
if def UART_INIT
jUART_RX_RDY	jp UART_RX_RDY
jUART_RX		jp UART_RX
jUART_RX_CHK	jp UART_RX_CHK
jUART_TX_RDY	jp UART_TX_RDY
jUART_TX		jp UART_TX
jUART_PRNT_SP	jp UART_PRNT_SP
jUART_PRNT_STR	jp UART_PRNT_STR
endif
jj:	jp 0

epp_go_ram:	ld a,1
		out (memmap),a
		jp $0

JUMPTAB_END:	equ $
