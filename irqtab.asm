; Interrupt vector table. Default all handlers to just return from int.
; when adding interrupt handler, it needs an init routine to add pointer to ISR
; to vector table as well as ISR itself

IRQTABR:		; irq tab for IM2 and for NMI, IRQ

SIOB_TX_BUF_E:	DEFW irq_x_1	; 00 SIO B TX buffer empty
SIOB_CHG_STAT:	DEFW irq_x_1	; 01 SIO B change/status
SIOB_RX_CH_CON:	DEFW irq_x_1	; 02 SIO B RX char condition available
SIOB_RX_SPEC:	DEFW irq_x_1	; 03 SIO B RX special condition

SIOA_TX_BUF_E:	DEFW irq_x_1	; 04 SIO A TX buffer empty
SIOA_CHG_STAT:	DEFW irq_x_1	; 05 SIO A TX change/status
SIOA_RX_CH_CON:	DEFW irq_x_1	; 06 SIO A RX char condition 
SIOA_RX_SPEC:	DEFW irq_x_1	; 07 SIO A RX special condition

CTC_CH0_INT:	DEFW CTC_T0_ISR	; 08 
CTC_CH1_INT:	DEFW CTC_T1_ISR	; 09 
CTC_CH2_INT:	DEFW irq_x_1	; 0A 
CTC_CH3_INT:	DEFW irq_x_1	; 0B 

PIOA_INTV:		DEFW irq_x_1	; 0C 
PIOB_INTV:		DEFW irq_x_1	; 0D 

;		org IRQTABR+$ff-6

IRQV:		JP irq_end
NMIV:		JP irq_end

IRQTABEND:	equ $

