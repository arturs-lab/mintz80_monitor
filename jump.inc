		include "CONSTANTS.asm"

jJUMPTAB_INIT:	equ JUMPTAB+$00	; initialize jump table
jCON_RX		equ JUMPTAB+$03	; receive char on console
jCON_TX		equ JUMPTAB+$06	; send char on console
jCON_PRT_STR_SP	equ JUMPTAB+$09	; print string pointed to by stack
jCON_PRT_STR:	equ JUMPTAB+$0c	; print string pointed to by HL
jCON_PRT_CHAR:	equ JUMPTAB+$0f	; print character
jCON_PRT_NL:	equ JUMPTAB+$12	; print CR LF
jCON_GET_CHAR	equ JUMPTAB+$15	; get character
jCON_CHAR_ISHEX	equ JUMPTAB+$18	; is character valid hex
jCON_GETHEXNIB	equ JUMPTAB+$1b	; get hex nibble
jCON_GETHEXBYTE	equ JUMPTAB+$1e	; get hex byte
jCON_GETHEXWORD	equ JUMPTAB+$21	; get hex word
jCON_NIB2CHAR	equ JUMPTAB+$24	; nibble to character
jCON_SHFTNIB	equ JUMPTAB+$27
jCON_PRINTHNIB:	equ JUMPTAB+$2a
jCON_PRINTHBYTE:	equ JUMPTAB+$2d
jCON_PRINTHWORD:	equ JUMPTAB+$30
jCON_CHAR2NIB	equ JUMPTAB+$33
jCON_CHARS2BYTE	equ JUMPTAB+$36
jCON_TO_UPPER	equ JUMPTAB+$39
jdel00		equ JUMPTAB+$3c
jdelay		equ JUMPTAB+$3f
jchime		equ JUMPTAB+$42
jbeep		equ JUMPTAB+$45
jymzinit	equ JUMPTAB+$48
jymzwr		equ JUMPTAB+$4b
jymzsetreg	equ JUMPTAB+$4e
jepp_prep	equ JUMPTAB+$51
jepp_upda	equ JUMPTAB+$54
jepp_prog	equ JUMPTAB+$57
jCTC_INIT_ALL	equ JUMPTAB+$5a
jCTC0_INIT	equ JUMPTAB+$5d
jCTC1_INIT	equ JUMPTAB+$60
jCTC2_INIT	equ JUMPTAB+$63
jCTC3_INIT	equ JUMPTAB+$66
jCTC_TC_INIT	equ JUMPTAB+$69
jSIO_INIT	equ JUMPTAB+$6c
jSIOA_INIT	equ JUMPTAB+$6f
jSIOB_INIT	equ JUMPTAB+$72
jSIO_A_INT_SET	equ JUMPTAB+$75
jSIOA_RTS_OFF	equ JUMPTAB+$78
jSIOA_RTS_ON	equ JUMPTAB+$7b
jSIO_A_DI	equ JUMPTAB+$7e
jSIO_A_EI	equ JUMPTAB+$81
jSIOA_PRNT_SP	equ JUMPTAB+$84
jSIOA_PRNT_STR	equ JUMPTAB+$87
jSIOA_TX_RDY	equ JUMPTAB+$8a
jSIOA_TX	equ JUMPTAB+$8d
jSIOA_RX_WAIT	equ JUMPTAB+$90
jSIOA_RX_CHK	equ JUMPTAB+$93
jSIOA_RX	equ JUMPTAB+$96
jSIOB_PRNT_SP	equ JUMPTAB+$99
jSIOB_PRNT_STR	equ JUMPTAB+$9c
jSIOB_TX_RDY	equ JUMPTAB+$9f
jSIOB_TX	equ JUMPTAB+$a2
jSIOB_RX_WAIT	equ JUMPTAB+$a5
jSIOB_RX_CHK	equ JUMPTAB+$a8
jSIOB_RX	equ JUMPTAB+$ab
jPIO_INIT	equ JUMPTAB+$ae
jCF_INIT	equ JUMPTAB+$b1
jCF_LP_BUSY	equ JUMPTAB+$b4
jCF_LP_CMD_RDY	equ JUMPTAB+$b7
jCF_LP_DAT_RDY	equ JUMPTAB+$ba
jCF_RD_CMD	equ JUMPTAB+$bd
jCF_RD_SECT	equ JUMPTAB+$c0
jCF_WR_CMD	equ JUMPTAB+$c3
jCF_WR_SECT	equ JUMPTAB+$c6
jCF_SETUP_LBA	equ JUMPTAB+$c9
jCF_SETUP_PART	equ JUMPTAB+$cc
jCF_PART_NEXT	equ JUMPTAB+$cf
jCF_SYSLD	equ JUMPTAB+$d2
jUART_RX_RDY	equ JUMPTAB+$d5
jUART_RX	equ JUMPTAB+$d8
jUART_RX_CHK	equ JUMPTAB+$db
jUART_TX_RDY	equ JUMPTAB+$de
jUART_TX	equ JUMPTAB+$e1
jUART_PRNT_SP	equ JUMPTAB+$e4
jUART_PRNT_STR	equ JUMPTAB+$e7
