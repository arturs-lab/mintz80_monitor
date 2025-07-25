epp_srcaddr:	db $0d,$0a,"Address of source data ",0
epp_tgtaddr:	db $0d,$0a,"Address of destination ",0
epp_numbyte:	db $0d,$0a,"Data length ",0
epp_page:		db $0d,$0a,"Page to write to (00 or 02) ",0

; we intentionally make this a two step process so that
; user can alter addresses and data length as needed
; before executing programming routine

; copy EEPROM programming code into RAM
epp_prep	push hl
		push de
		push bc
		
		ld hl,epp_prog
		ld de,epp_tmp
		ld bc,epp_end-epp_prog
		ldir

		pop bc
		pop de
		pop hl
		ret

; update source addr
epp_upda:	push hl
		push af

		ld hl,epp_srcaddr
		call PRINT_STRING
		call GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		RET NZ
		ld (epp_tmp+epp_srca-epp_prog+1),hl

		; update target addr
		ld hl,epp_tgtaddr
		call PRINT_STRING
		call GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd1		; skip if invalid entry
		ld (epp_tmp+epp_tgta-epp_prog+1),hl

		; update byte count
epp_upd1:	ld hl,epp_numbyte
		call PRINT_STRING
		call GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd2		; skip if invalid entry
		ld (epp_tmp+epp_lena-epp_prog+1),hl

		; update target page
epp_upd2:	ld hl,epp_page
		call PRINT_STRING
		call GETHEXBYTE
		ld l,a			; save temporarily
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd3		; skip if invalid entry
		ld a,l			; restore
		ld (epp_tmp+epp_banka-epp_prog+1),a

epp_upd3:	pop af
		pop hl
		ret

; Execute programming and jump to zero after
epp_prog:	push hl
		push de
		push bc
		push af
epp_banka:	ld a,epp_bank	; by default program second bank of boot EEPROM and jump to it.
		out (memmap),a	; if programming fails, can just reset and boot to previous EEPROM
epp_tgta:	ld hl,epp_tgt	; source of data to be programmed
epp_srca:	ld de,epp_src	; target address
epp_lena:	ld bc,epp_len	; number of bytes

epp_p1:	ld a,b		; check if remaining bytes = 0
		or c
		jr z,epp_exit	; yes, end the procedure
		ld a,(de)		; no, fetch next byte
		ld (hl),a		; store it in target address
epp_p2:	push af		; save programmed byte
		out (beepr),a
		ld a,epp_del	; delay before reading back byte
epp_delay:	push af			; count delay
epp_del1:	dec a
		jr nz,epp_del1		; repeat till 0
		pop af
		dec a
		jr nz,epp_delay
		pop af		; restore programmed byte
		cp (hl)		; compare with EEPROM content
		jr nz,epp_p2	; repeat till readback = programmed data
		inc hl
		inc de
		dec bc
		jr epp_p1

epp_exit:	ld ($ff60),hl
		ld ($ff62),de
		ld ($ff64),bc
		ld ($ff66),a
		pop af
		pop bc
		pop de
		pop hl
		jp $0000		; since we've potentially changed the location of code that got us here, just start over

deltest:	ld b,0
dt1:		out (beepr),a
		ld a,epp_del	; delay before reading back byte
		call epp_delay	; about 100ms
		dec b
		jr nz,dt1
		ret

epp_end:	equ $
