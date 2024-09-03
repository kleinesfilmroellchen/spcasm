org $200
loop:
	nop
	jmp loop
	nop

org $FF00
startpos
	jmp loop
