	include "jump.inc"

	org $c000

bootloader:	db "Z",$80

	call jCON_PRT_STR_SP	; 
zoWarnFlow = false
	db "CF Load SYS",0
zoWarnFlow = true
	call jCF_SYSLD		; load CF sectors into $c200-$ffff, this call destroys value of CFSECT_BUF
	ret nz			; if failed loading system, jump to main monitor loop
	ld hl,CFSECT_BUF_V+$200
	ld a,(hl)
	cp a,0			; if (CFSECT_BUF + $0200) = 0 then no code to run
	ret z
	cp a,$ff			; if (CFSECT_BUF + $0200) = $ff then no code to run
	ret z		; otherwise presume this may be a valid code that can be jumped to

	ld hl,CFSECT_BUF_V+$3FD0	; calculate location of label
	ld a,(hl)
	cp a," "			; if (CFSECT_BUF_V+$3FE0) < " " then no label to print
	jr c,chksum_calc
	cp a,$7f			; if (CFSECT_BUF_V+$3FE0) > "" then no label to print
	jr nc,chksum_calc
	call jCON_PRT_STR_SP	; otherwise print label
zoWarnFlow = false
	db "Found: ",0
zoWarnFlow = true
	call jCON_PRT_STR

chksum_calc:	ld hl,CFSECT_BUF_V+$200	; calculate and print checksum
	ld (MVADDR+0),hl
	ld bc,$3dcf
	add hl,bc
	ld (MVADDR+2),hl
	call jCCKSM_DO

	ld de,hl			; preserve checksum, high byte of checksum is in A
	ld hl,CFSECT_BUF_V+$3ffd	; calculate location of checksum in loaded data

	cp a,(hl)			; check MSB of checksum
	jr nz,cksm_invalid
	inc hl
	ld a,d
	cp a,(hl)			; check MSB of checksum
	jr nz,cksm_invalid
	inc hl
	ld a,e
	cp a,(hl)			; check MSB of checksum
	jr nz,cksm_invalid

; Checksum good, confirm running
	ld hl,($fff7)	; data length from data section
	call jCON_PRINTHWORD
	call jCON_PRT_STR_SP	; announce loading code
zoWarnFlow = false
	db " bytes -> ",0
zoWarnFlow = true
	ld hl,($fffb)	; copy target from data section
	call jCON_PRINTHWORD
	call jCON_PRT_STR_SP	; announce loading code
zoWarnFlow = false
	db " , JMP ",0
zoWarnFlow = true
	ld hl,($fff9)	; jump destination from data section
	call jCON_PRINTHWORD

	call jCON_PRT_STR_SP	
zoWarnFlow = false
	db $0d,$0a,"Run? (y/n) ",0
zoWarnFlow = true
	call jCON_GET_CHAR
	cp a,"Y"
	ret nz		; anything but "y"
; if checksum was valid, then we presume that other data is also valid
	ld hl,($fff7)	; data length from data section
	ld (dta_size),hl
	ld hl,($fff9)	; jump destination from data section
	ld (jmp_dest),hl
	ld hl,($fffb)	; copy target from data section
	ld (boot_dest),hl
	jr boot_go

cksm_invalid:	call jCON_PRT_STR_SP	; announce invalid checksum
zoWarnFlow = false
	db "Chksum invalid. Still want to run? (y/n) ",0
zoWarnFlow = true
	call jCON_GET_CHAR
	cp a,"Y"
	ret nz
; even if we get Y here, since checksum invalid, we don't trust other data
; there and go with bootloader's defaults

boot_go:	call jCON_PRT_STR_SP	; announce loading code
zoWarnFlow = false
	db $0d,$0a,"Loading ",0
zoWarnFlow = true
	ld hl,(dta_size)
	call jCON_PRINTHWORD
	call jCON_PRT_STR_SP
zoWarnFlow = false
	db " bytes at ",0
zoWarnFlow = true
	ld hl,(boot_dest)
	call jCON_PRINTHWORD
	call jCON_PRT_STR_SP
zoWarnFlow = false
	db " and JMP ",0
zoWarnFlow = true
	ld hl,(jmp_dest)
	call jCON_PRINTHWORD
	call jCON_PRT_NL

sw_mem:	di		; Disable interrupts before we switch out memory containing ISR
	ld a,03
	out ($d8),a
	out ($d8),a	; do it twice to be sure
	in a,($d8)		; read it back
	and $07		; only keep lower 3 bits
	cp $03		; should be 3
	jr nz,boot_err
	ld a,03
	out ($d9),a
	out ($d9),a	; do it twice to be sure
	in a,($d9)		; read it back
	and $07		; only keep lower 3 bits
	cp $03		; should be 3
	jr nz,boot_err

boot_cnt:	ld hl,CFSECT_BUF_V+$200
	ld de,(boot_dest)		; if found code, copy it to $0000-$3cff
	ld bc,(dta_size)
	ldir

;would be nice to print jump message here, but monitor may be switched out now

	ld hl,(jmp_dest)	; load destination of jump
	jp (hl)			; and jump to it

boot_err:	ld a,00		; restore default values
	out ($d8),a
	out ($d8),a	; do it twice to be sure
	ld a,01
	out ($d9),a
	out ($d9),a	; do it twice to be sure
; now that hopefully monitor is back, we can try to print message before returning
	call jCON_PRT_STR_SP	; announce failure
zoWarnFlow = false
	db $0d,$0a,"Failed to switch to page 3",0
zoWarnFlow = true
	ret

	org $c1a0
code_label:	db "Universal Bootloader",0		; label of this program for monitor loader to display

	org $c1b8
dta_size	dw $3e00		; default $3e00
jmp_dest	dw $0000
boot_dest	dw $0000		; destination address of boot code

endprog	equ $

	output_bin "bootloader_0000.bin",bootloader,endprog-bootloader 
	output_intel "bootloader_0000.hex",bootloader,endprog-bootloader
	output_list "bootloader_0000.lst"
