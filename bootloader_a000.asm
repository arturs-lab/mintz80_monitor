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
	ld bc,$1d00
	ldir

	ld hl,(boot_dest)	; calculate and print checksum
	ld (MVADDR+0),hl
	ld bc,$1d00-1
	add hl,bc
	ld (MVADDR+2),hl
	call jCCKSM_DO

	call jCON_PRT_STR_SP	; announce jumping to loaded code
zoWarnFlow = false
	db "Jumping to ",0
zoWarnFlow = true
	ld hl,(boot_dest)
	call jCON_PRINTHWORD
	call jCON_PRT_NL

	jp (hl)			; and jump to beginning of it

boot_dest	dw $a000		; destination address of boot code

endprog	equ $

	output_bin "bootloader.bin",bootloader,endprog-bootloader 
	output_intel "bootloader.hex",bootloader,endprog-bootloader
	output_list "bootloader.lst"
