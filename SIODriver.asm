;***************************************************************************
;  PROGRAM:			SIODriver        
;  PURPOSE:			Subroutines for Z80 SIO
;  CREATE DATE :	12 Sep 25
;  Version:			1.0
;***************************************************************************

		
; initialize both SIO channels
SIO_INIT:	call SIOA_INIT	; first init SIO A
		call SIOB_INIT	; first init SIO A
		call jSIO_A_INT_SET	; initialize SIOA interrupts
		call jSIO_A_EI		; more interrupt code
		ret

;***************************************************************************
;SIOA_INIT
;Function: Initialize the SIOA to BAUD Rate 4800 
;the followings are settings for channel A
;***************************************************************************
SIOA_INIT:	ld a,00110000b      ; write into WR0: error reset, select WR0
        out (SIO_CA),a
        ld a,00011000b      ; write into WR0: channel reset
        out (SIO_CA),a
        ld a,00000100b      ; write into WR0: select WR4
        out (SIO_CA),a
        ld a,00000100b      ; write into WR4: presc. 1x, 1 stop bit, no parity
        out (SIO_CA),a
        ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101000b      ; write into WR5: DTR on, TX 8 bits, BREAK off, TX on, RTS off
        out (SIO_CA),a
		ret

;***************************************************************************
;SIOB_INIT
;Function: Initialize the SIOB to BAUD Rate 4800 
;the followings are settings for channel B
;***************************************************************************
SIOB_INIT:	ld a,00000001b      ; write into WR0: select WR1
        out (SIO_CB),a
        ld a,00000100b      ; write into WR0: status affects interrupt vectors
        out (SIO_CB),a
        ld a,00000010b      ; write into WR0: select WR2
        out (SIO_CB),a
        ld a,SIO_INT_VECT   ; write into WR2: set interrupt vector, but bits D3/D2/D1 of this vector
                            ; will be affected by the channel & condition that raised the interrupt
                            ; (see datasheet): in our example, 0x0C for Ch.A receiving a char, 0x0E
                            ; for special conditions
        out (SIO_CB),a
		ret

; set up interrupts for channel A
SIO_A_INT_SET:
        ; the following are settings for channel A
        ld a,01h            ; write into WR0: select WR1
        out (SIO_CA),a
        ld a,00011000b      ; interrupts on every RX char; parity is no special condition;
                            ; buffer overrun is special condition
        out (SIO_CA),a
		ret

;-------------------------------------------------------------------------------
; serial management

SIOA_RTS_OFF:	ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101000b      ; 8 bits/TX char; TX enable; RTS disable
        out (SIO_CA),a
        ret

SIOA_RTS_ON:	ld a,00000101b      ; write into WR0: select WR5
        out (SIO_CA),a
        ld a,11101010b      ; 8 bits/TX char; TX enable; RTS enable
        out (SIO_CA),a
        ret

        ;disable SIO channel A RX
SIO_A_DI:	ld a,00000011b      ; write into WR0: select WR3
        out (SIO_CA),a
        ld a,00001100b      ; write into WR3: RX disable;
        out (SIO_CA),a
        ret


; enable interrupt for SIOA
        ;enable SIO channel A RX
SIO_A_EI:	ld a,00000011b      ; write into WR0: select WR3
        out (SIO_CA),a
        ld a,11000001b      ; 8 bits/RX char; auto enable OFF; RX enable
        out (SIO_CA),a
        ret

;***************************************************************************
;SIOA_PRNT_SP:
;Function: Print out string starting at MEM location (SP) to SIOA
; put string to print, terminated by EOS immediately after the CALL instruction
; calling this procedure
;***************************************************************************
SIOA_PRNT_SP:	ex (sp),hl                 
			call SIOA_PRNT_STR
			inc hl
			ex (sp),hl
			ret

;***************************************************************************
;SIOA_PRNT_STR:
;Function: Print out string starting at MEM location (HL) to 16550 UART
;***************************************************************************
SIOA_PRNT_STR:	PUSH	AF
SIOAPRNTSTRLP:	LD		A,(HL)
		CP		EOS					;Test for end byte
		JR		Z,SIOA_END_PRNT_STR	;Jump if end byte is found
		CALL	SIOA_TX
		INC		HL					;Increment pointer to next char
		JR		SIOAPRNTSTRLP	;Transmit loop
SIOA_END_PRNT_STR:	POP		AF
		RET	 
			 	
;***************************************************************************
;SIOA_TX_READY blocking
;Function: wait for SIOA to be ready to transmit
;***************************************************************************
SIOA_TX_RDY:	PUSH 	AF
SIOA_TX_RD1:	ld a,1
		out (SIO_CA),a
		in a,(SIO_CA)
		bit 0,a
		jr z,SIOA_TX_RD1
		POP     AF
		RET
	
;***************************************************************************
;SIOA_TX
;Function: Transmit character in A to SIOA
;***************************************************************************
SIOA_TX:		CALL  SIOA_TX_RDY			;Make sure UART is ready to receive
			out (SIO_DA),a      			;Transmit character in A to UART
			RET
				
;***************************************************************************
;SIOA_RX_WAIT blocking
;Function: wait for SIOA to receive char
;***************************************************************************
SIOA_RX_WAIT:	PUSH 	AF					
SIOA_RX_WAIT_LP:	sub a               ;clear a, write into WR0: select RR0
		out (SIO_CA),a
		in a,(SIO_CA)       ;read RRx
		bit 0,a
		jr z,SIOA_RX_WAIT_LP	;if any rx char left in rx buffer
			POP     AF
			RET
	
;***************************************************************************
;SIOA_RX_CHK
;Function: Non-blocking receive char
; returns 0 if no data or char if data present
;***************************************************************************
SIOA_RX_CHK:	sub a               ;clear a, write into WR0: select RR0
		out (SIO_CA),a
		in a,(SIO_CA)       ;read RRx
		bit 0,a
		ret z			;if any rx char left in rx buffer
		in a,(SIO_DA)       ;read that char
		ret			; return status


;***************************************************************************
;UART_RX
;Function: Receive character in UART to A
; either wait for it if called at SIOA_RX
; or just fetch it without waiting if called at SIOA_RX1
; the latter is in case we already tested for and know char is available
;***************************************************************************
SIOA_RX:	CALL  SIOA_RX_WAIT			;wait to receive char
SIOA_RX1:	in a,(SIO_DA)       ;read that char
		RET			

;#######
;***************************************************************************
;SIOB_PRNT_SP:
;Function: Print out string starting at MEM location (SP) to SIOB
; put string to print, terminated by EOS immediately after the CALL instruction
; calling this procedure
;***************************************************************************
SIOB_PRNT_SP:	ex (sp),hl                 
			call SIOB_PRNT_STR
			inc hl
			ex (sp),hl
			ret

;***************************************************************************
;SIOB_PRNT_STR:
;Function: Print out string starting at MEM location (HL) to 16550 UART
;***************************************************************************
SIOB_PRNT_STR:	PUSH	AF
SIOBPRNTSTRLP:	LD		A,(HL)
		CP		EOS					;Test for end byte
		JR		Z,SIOB_END_PRNT_STR	;Jump if end byte is found
		CALL	SIOB_TX
		INC		HL					;Increment pointer to next char
		JR		SIOBPRNTSTRLP	;Transmit loop
SIOB_END_PRNT_STR:	POP		AF
		RET	 
			 	
;***************************************************************************
;SIOB_TX_READY blocking
;Function: wait for SIOB to be ready to transmit
;***************************************************************************
SIOB_TX_RDY:	PUSH 	AF
SIOB_TX_RD1:	ld a,1
		out (SIO_CB),a
		in a,(SIO_CB)
		bit 0,a
		jr z,SIOB_TX_RD1
		POP     AF
		RET
	
;***************************************************************************
;SIOB_TX
;Function: Transmit character in A to SIOB
;***************************************************************************
SIOB_TX:		CALL  SIOB_TX_RDY			;Make sure UART is ready to receive
			out (SIO_DB),a      			;Transmit character in A to UART
			RET
				
;***************************************************************************
;SIOB_RX_WAIT blocking
;Function: wait for SIOB to receive char
;***************************************************************************
SIOB_RX_WAIT:	PUSH 	AF					
SIOB_RX_WAIT_LP:	sub a               ;clear a, write into WR0: select RR0
		out (SIO_CB),a
		in a,(SIO_CB)       ;read RRx
		bit 0,a
		jr z,SIOB_RX_WAIT_LP	;if any rx char left in rx buffer
			POP     AF
			RET
	
;***************************************************************************
;SIOB_RX_CHK
;Function: Non-blocking receive char
; returns 0 if no data or char if data present
;***************************************************************************
SIOB_RX_CHK:	sub a               ;clear a, write into WR0: select RR0
		out (SIO_CB),a
		in a,(SIO_CB)       ;read RRx
		bit 0,a
		ret z			;if any rx char left in rx buffer
		in a,(SIO_DB)       ;read that char
		ret			; return status


;***************************************************************************
;UART_RX
;Function: Receive character in UART to A
; either wait for it if called at SIOB_RX
; or just fetch it without waiting if called at SIOB_RX1
; the latter is in case we already tested for and know char is available
;***************************************************************************
SIOB_RX:	CALL  SIOB_RX_WAIT			;wait to receive char
SIOB_RX1:	in a,(SIO_DB)       ;read that char
		RET			


