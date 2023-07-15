
; If you want to switch between segments, you can do this:
org $200

some_code:
	nop
; [...]
	ret

pushpc

org $5A0

some_subroutine:
	nop
	nop
; [...]
	ret

; Switching to the lower segment for one function...
pushpc
pullpc

do_stuff:
	ei
	nop
; [...]
	dbnz y, do_stuff
	ret

pushpc
pullpc
; ... and back to the higher segment!

some_other_subroutine:
	nop
; [...]
