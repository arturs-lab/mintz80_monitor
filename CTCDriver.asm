CTC_INIT_ALL: push af
		call CTC_TC_INIT
		call CTC0_INIT
		call CTC1_INIT
		call CTC2_INIT
		call CTC3_INIT
		pop af
		ret

;CH0 divides CPU CLK by (256*CTC_CH0_TC) providing a clock signal at TO0. TO0 is connected to TRG1.
; T01 outputs f= CPU_CLK/(256*CTC_CH0_TC) => 9.216MHz / ( 256 * 180 ) => 200Hz
CTC0_INIT:	ld a,(CTC_CH0_CNF)      ; interrupt off; timer mode; prescaler=256; don't care ext; automatic trigger;
                            ; time constant follows; cont. operation; command word
        out (CTC_CH0),a
        ld a,(CTC_CH0_TC)           ; time constant
        out (CTC_CH0),a

;init CH1
;CH1 disabled
CTC1_INIT:	ld a,(CTC_CH1_CNF)      ; interrupt off, timer mode, prescaler=16, don't care ext. TRG edge,
                            ; start timer on loading constant, time constant follows, software reset, command word
        out (CTC_CH1),a         ; CH1 is halted
        ld A,(CTC_CH1_TC)           ; time constant 56d
        out (CTC_CH1),a         ; loaded into channel 1

;init CH2
;CH2 divides CLK/TRG2 clock providing a clock signal at TO2.
; T02 outputs f= CLK/TRG / 0x34 => 4MHz / 52 => 38461
CTC2_INIT:	ld a,(CTC_CH2_CNF)      ; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CTC_CH2),a
        ld A,(CTC_CH2_TC)           ; time constant 56d
        out (CTC_CH2),a         ; loaded into channel 2

;init CH3
;CH3 divides CLK/TRG3 clock providing a clock signal at TO3.
; T03 outputs f= CLK/TRG / 4  => 4MHz / 4 => 1MHz
CTC3_INIT:	ld a,(CTC_CH3_CNF)      ; interrupt off, counter mode, prescaler=256 (doesn't matter), ext. start,
                            ; start upon loading time constant, time constant follows,sw reset, command word
        out (CTC_CH3),a
        ld A,(CTC_CH3_TC)           ; time constant 56d
        out (CTC_CH3),a         ; loaded into channel 3
        ret


CTC_TC_INIT: ld hl,CTC_DEFAULTS
		ld de,CTC_CH0_CNF
		ld bc,8
		ldir
		ret

; these are configured in CONSTANTS.asm
CTC_DEFAULTS:	db CTC_CH0_CNFV,CTC_CH1_CNFV,CTC_CH2_CNFV,CTC_CH3_CNFV
			db CTC_CH0_TV,CTC_CH1_TV,CTC_CH2_TV,CTC_CH3_TV
