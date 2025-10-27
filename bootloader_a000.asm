	include "jump.inc"

	org $c000

bootloader:	db "Z",$80

	call jCON_PRT_STR_SP	; 
zoWarnFlow = false
	db "CF Loading SYS sectors",0
zoWarnFlow = true
	call jCF_SYSLD		; load CF sectors into $c200-$ffff, this call destroys value of CFSECT_BUF
	ret nz			; if failed loading system, jump to main monitor loop
	ld hl,CFSECT_BUF_V+$200
	ld a,(hl)
	cp a,0			; if (CFSECT_BUF + $0200) = 0 then no code to run
	ret z
	cp a,$ff			; if (CFSECT_BUF + $0200) = $ff then no code to run
	ret z		; otherwise presume this may be a valid code that can be jumped to

	ld de,(boot_dest)		; if found code, copy it to $a000-$bcff
	ld bc,$2000
	ldir

	ld hl,(boot_dest)	; calculate and print checksum
	ld (MVADDR+0),hl
	ld bc,$1d00-1
	add hl,bc
	ld (MVADDR+2),hl
	call jCCKSM_DO

	ld de,hl			; preserve checksum
	ld hl,(boot_dest)	; calculate location of checksum in loaded data
	ld bc,$2000-3
	add hl,bc

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
	jr boot_go

cksm_invalid:	call jCON_PRT_STR_SP	; announce invalid checksum
zoWarnFlow = false
	db "Checksum invalid. Do you still want to run this code? (y/n) ",0
zoWarnFlow = true
	call jCON_GET_CHAR
	cp a,"Y"
	ret nz

boot_go:	call jCON_PRT_STR_SP	; announce jumping to loaded code
zoWarnFlow = false
	db $0d,$0a,"Jumping to ",0
zoWarnFlow = true
	ld hl,(boot_dest)
	call jCON_PRINTHWORD
	call jCON_PRT_NL

	jp (hl)			; and jump to beginning of it

boot_dest	dw $a000		; destination address of boot code

endprog	equ $

	output_bin "bootloader_a000.bin",bootloader,endprog-bootloader 
	output_intel "bootloader_a000.hex",bootloader,endprog-bootloader
	output_list "bootloader_a000.lst"
