; Interrupt vector table

IRQTABR:		; irq tab for IM2 and for NMI, IRQ

SIOB_TX_BUF_E:	DEFW SIOV+$0	; SIO B TX buffer empty
SIOB_CHG_STAT:	DEFW SIOV+$2	; SIO B change/status
SIOB_RX_CH_CON:	DEFW SIOV+$4	; SIO B RX char condition available
SIOB_RX_SPEC:	DEFW SIOV+$6	; SIO B RX special condition

SIOA_TX_BUF_E:	DEFW SIOV+$8	; SIO A TX buffer empty
SIOA_CHG_STAT:	DEFW SIOV+$a	; SIO A TX change/status
SIOA_RX_CH_CON:	DEFW SIOV+$c	; SIO A RX char condition 
SIOA_RX_SPEC:	DEFW SIOV+$e	; SIO A RX special condition

CTC_CH0_INT:	DEFW CTCV+0
CTC_CH1_INT:	DEFW CTCV+1
CTC_CH2_INT:	DEFW CTCV+2
CTC_CH3_INT:	DEFW CTCV+3

PIOA_INTV:		DEFW PIOV+0
PIOB_INTV:		DEFW PIOV+1

;		org IRQTABR+$ff-6

IRQV:		JP irq_end
NMIV:		JP irq_end

IRQTABEND:	equ $


