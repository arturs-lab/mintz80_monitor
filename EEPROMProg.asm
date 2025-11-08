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
		
		ld hl,epp_prog			; first copy epp_prog into temp space
		ld de,epp_tmp
		ld bc,epp_end-epp_prog
		ldir

		ld hl,epp_switch		; copy epp_switch and epp_jmp_bnk behind epp_prog
		ld bc,epp_prog-epp_switch
		ldir

		pop bc
		pop de
		pop hl
		ret

; update source addr
epp_upda:	push hl
		push af

		ld hl,epp_srcaddr
		call CON_PRT_STR
		call CON_GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		RET NZ
		ld (epp_tmp+epp_srca-epp_prog+1),hl

		; update target addr
		ld hl,epp_tgtaddr
		call CON_PRT_STR
		call CON_GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd1		; skip if invalid entry
		ld (epp_tmp+epp_tgta-epp_prog+1),hl
		LD (MVADDR+0), HL

		; update byte count
epp_upd1:	ld hl,epp_numbyte
		call CON_PRT_STR
		call CON_GETHEXWORD
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd2		; skip if invalid entry
		ld (epp_tmp+epp_lena-epp_prog+1),hl
		ld bc,(MVADDR+0)
		add hl,bc
		dec hl
		LD (MVADDR+2), HL

		; update target page
epp_upd2:	ld hl,epp_page
		call CON_PRT_STR
		call CON_GETHEXBYTE
		ld l,a			; save temporarily
		LD A, (ERRFLAG)
		CP E_NONE
		jr NZ,epp_upd3		; skip if invalid entry
		ld a,l			; restore
		ld (epp_tmp+epp_banka-epp_prog+1),a
		ld (epp_tmp+epp_end-epp_prog+epp_jmp_bnk-epp_switch+1),a	; funny math because we're copying epp_jmp_bnk behind epp_prog in temp space

epp_upd3:	pop af
		pop hl
		ret

epp_switch:	in a,(memmap)	; toggle other bank. call this at $ffnn after executing epp_prep
		xor a,$02
		out (memmap),a
		jp $0

; call this at $ffnn after executing epp_prep
epp_jmp_bnk:	ld a,epp_bank	; jump to whatever bank is indicated in epp_bank
		out (memmap),a	; if programming fails, can just reset and boot to previous EEPROM
		jp $0000		; since we've potentially changed the location of code that got us here, just start over


; Execute programming and jump to zero after
epp_prog:	push hl
		push de
		push bc
		push af
		call CON_PRT_NL
epp_banka:	ld a,epp_bank	; by default program second bank of boot EEPROM and jump to it.
		out (memmap),a	; if programming fails, can just reset and boot to previous EEPROM
epp_tgta:	ld hl,epp_tgt	; target of data to be programmed
epp_srca:	ld de,epp_src	; source address
epp_lena:	ld bc,epp_len	; number of bytes

epp_p1:	ld a,b		; check if remaining bytes = 0
		or c
		jr z,epp_exit	; yes, end the procedure
		ld a,(de)		; no, fetch next byte
		cp (hl)		; compare with EEPROM content
		jr z,epp_p4	; skip programming if already equal
		ld (hl),a		; store it in target address
		jr epp_p3
epp_p2:	push af		; save programmed byte
		ld a,epp_del	; delay before reading back byte
epp_delay:	push af			; count delay
epp_del1:	dec a
		jr nz,epp_del1		; repeat till 0
		pop af
		dec a
		jr nz,epp_delay
		pop af		; restore programmed byte
epp_p3:	out (beepr),a
		cp (hl)		; compare with EEPROM content
		jr nz,epp_p2	; repeat till readback = programmed data
epp_p4:	inc hl
		inc de
		dec bc
	if def ROM_BOTTOM_a000
		ld a,c
		or a
		jr nz,epp_p1
		call CON_PRINTHWORD
		call CON_PRT_NL
	endif
	if def ROM_BOTTOM_c000
		ld a,c
		or a
		jr nz,epp_p1
		call CON_PRINTHWORD
		call CON_PRT_NL
	endif
		jr epp_p1

; this was just for troubleshooting
;epp_exit:	ld (MONVARS+$f0),hl
;		ld (MONVARS+$f2),de
;		ld (MONVARS+$f4),bc
;		ld (MONVARS+$f6),a
		
epp_exit:	pop af
		pop bc
		pop de
		pop hl
	if def ROM_BOTTOM_c000
		jp CCKSM_DO
	elseif def ROM_BOTTOM_a000
		jp CCKSM_DO
	else
		jp $0000		; since we've potentially changed the location of code that got us here, just start over
	endif

epp_end:	equ $

;deltest:	ld b,0
;dt1:		out (beepr),a
;		ld a,epp_del	; delay before reading back byte
;		call epp_delay	; about 100ms
;		dec b
;		jr nz,dt1
;		ret

