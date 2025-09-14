CTC_INIT_ALL: push af
		call CTC_TC_INIT
		call CTC0_INIT
		call CTC1_INIT
		call CTC2_INIT
		call CTC3_INIT
		pop af
		ret

;CH0 divides CPU CLK by (256*156) providing a clock signal at TO0. TO0 is connected to TRG1.
; T01 outputs f= CPU_CLK/(256*156) => 4MHz / ( 256 * 156 ) => 100Hz
CTC0_INIT:	ld a,00100111b      ; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
                            ; time constant follows; cont. operation; command word
        out (CTC_CH0),a
        ld a,(CTC_CH0_TC)           ; time constant
        out (CTC_CH0),a

;init CH1
;CH1 disabled
CTC1_INIT:	ld a,00000011b      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                            ; start timer on loading constant, no time constant follows, software reset, command word
        out (CTC_CH1),a         ; CH1 is halted
;        ld A,(CTC_CH1_TC)           ; time constant 56d
;        out (CTC_CH1),a         ; loaded into channel 1

;init CH2
;CH2 divides CLK/TRG2 clock providing a clock signal at TO2.
; T02 outputs f= CLK/TRG / 0x34 => 4MHz / 52 => 38461
CTC2_INIT:	ld a,01110111b      ; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CTC_CH2),a
        ld A,(CTC_CH2_TC)           ; time constant 56d
        out (CTC_CH2),a         ; loaded into channel 2

;init CH3
;CH3 divides CLK/TRG3 clock providing a clock signal at TO3.
; T03 outputs f= CLK/TRG / 4  => 4MHz / 4 => 1MHz
CTC3_INIT:	ld a,01110111b      ; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CTC_CH3),a
        ld A,(CTC_CH3_TC)           ; time constant 56d
        out (CTC_CH3),a         ; loaded into channel 3
        ret


CTC_TC_INIT: ld a,$9c
		ld (CTC_CH0_TC),a
		ld a,$9c
		ld (CTC_CH1_TC),a
		ld a,$d0
		ld (CTC_CH2_TC),a
		ld a,$d0
		ld (CTC_CH3_TC),a

		ret