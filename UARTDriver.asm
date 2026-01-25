;***************************************************************************
;  PROGRAM:			UARTDriver        
;  PURPOSE:			Subroutines for a 16550 UART
;  ASSEMBLER:		TASM 3.2        
;  LICENCE:			The MIT Licence
;  AUTHOR :			MCook
;  CREATE DATE :	06 May 15
;***************************************************************************

;The eight addresses that the 16550 resides in I/O space.
;Change to suit hardware.
UART0:       EQU    UART_BASE+00h			;Data in/out
UART1:       EQU    UART_BASE+01h            	;Check RX
UART2:       EQU    UART_BASE+02h            	;Interrupts
UART3:       EQU    UART_BASE+03h            	;Line control
UART4:       EQU    UART_BASE+04h            	;Modem control
UART5:       EQU    UART_BASE+05h            	;Line status
UART6:       EQU    UART_BASE+06h            	;Modem status
UART7:       EQU    UART_BASE+07h            	;Scratch register		
		
;***************************************************************************
; defaults with which to initialize UART variables at MONVARS
;***************************************************************************
UART_DFLT:	db UART_DLL_D,UART_DLM_D,UART_IER_D,UART_LCR_D,UART_MCR_D

;***************************************************************************
;UART_INIT
;Function: Initialize the UART to BAUD Rate CONBAUD (UARTCLK clock input)
;UART_INIT1
;don't initialize the defaults. can be called when changing parameters
;***************************************************************************
UART_INIT:		LD HL,UART_DFLT				; initialize default values in MONVARS
			LD DE,UART_DLL
			LD BC,SIOA_WR0-UART_DLL
			LDIR

UART_INIT1:	LD     A,80h				;Mask to Set DLAB Flag
			OUT    (UART3),A
			LD     A,(UART_DLL)			;Divisor = 12 @ 9600bps w/ 1.8432 Mhz
			OUT    (UART0),A			;Set BAUD rate to 9600
			LD     A,(UART_DLM)
			OUT    (UART1),A			;Set BAUD rate to 9600
			LD     A,(UART_LCR)
			OUT    (UART3),A			;Set 8-bit data, 1 stop bit, reset DLAB Flag
			LD	   A,(UART_IER)
			OUT    (UART1),A			;no interrupts
			RET		
		
;***************************************************************************
;UART_PRNT_SP:
;Function: Print out string starting at MEM location (SP) to 16550 UART
; put string to print, terminated by EOS immediately after the CALL instruction
; calling this procedure
;***************************************************************************
UART_PRNT_SP:	ex (sp),hl
			call UART_PRNT_STR
			inc hl
			ex (sp),hl
			ret

;***************************************************************************
;UART_PRNT_STR:
;Function: Print out string starting at MEM location (HL) to 16550 UART
;***************************************************************************
UART_PRNT_STR:	PUSH	AF
UARTPRNTSTRLP:
			LD		A,(HL)
            CP		EOS					;Test for end byte
            JR		Z,UART_END_PRNT_STR	;Jump if end byte is found
			CALL	UART_TX
            INC		HL					;Increment pointer to next char
            JR		UARTPRNTSTRLP	;Transmit loop
UART_END_PRNT_STR:
			POP		AF
			RET	 
			 	
;***************************************************************************
;UART_TX_READY
;Function: Check if UART is ready to transmit
;***************************************************************************
UART_TX_RDY:	PUSH 	AF
UARTTXRDY_LP:	IN		A,(UART5)			;Fetch the control register
			BIT 	5,A					;Bit will be set if UART is ready to send
			JR		Z,UARTTXRDY_LP		
			POP     AF
			RET
	
;***************************************************************************
;UART_TX
;Function: Transmit character in A to UART
;***************************************************************************
UART_TX:		CALL  UART_TX_RDY			;Make sure UART is ready to receive
			OUT   (UART0),A				;Transmit character in A to UART
			RET
				
;***************************************************************************
;UART_RX_READY
;Function: Wait for UART to receive a byte
;***************************************************************************
UART_RX_RDY:	PUSH 	AF					
UART_RXRDY_LP:	IN		A,(UART5)			;Fetch the control register
			BIT 	0,A					;Bit will be set if UART is ready to receive
			JR		Z,UART_RXRDY_LP		
			POP     AF
			RET
	
;***************************************************************************
;RX_CHK
;Function: Non-blocking receive check
;***************************************************************************
UART_RX_CHK:	IN		A,(UART5)			;Fetch the control register
			AND	1					;Mask other bits, has some char arrived?
			RET

;***************************************************************************
;UART_RX
;Function: Receive character in UART to A
;***************************************************************************
UART_RX:		CALL  UART_RX_RDY			;Make sure UART is ready to receive
			IN    A,(UART0)				;Receive character in UART to A
			RET			

;***************************************************************************
;uart_test
;Function: Check if UART is reachable. Make some noise if not
;***************************************************************************
uart_test	ld a,$55
	out (UART7),a
	in a,(UART7)
	cp a,$55
	jr nz,ut1
	ld a,$aa
	out (UART7),a
	in a,(UART7)
	cp a,$aa
	jr z,ut2
ut1:	ld bc,$0400	; bc = duration
	ld a,$04		; a = pitch
	call beep
ut2:	ret