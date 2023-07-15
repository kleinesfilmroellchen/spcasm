; S-SMP boot ROM (SNES/Super Famicom)
; These 64 bytes of on-chip ROM are accessible in $FFC0-FFFF in the S-SMP's address space.
; The code gets run by the S-SMP on reset and implements a basic data transfer protocol the 65c816 can use to initially upload samples and audio data.

org $FFC0

RESET:
   mov  x, #$EF      ;= CD EF      
   mov  sp, x        ;= BD         ; zerofill RAM at (0001h..00EFh)
   mov  a, #$00      ;= E8 00      ; (ie. excluding I/O Ports at F0h..FFh)
.clear:                            ; (though (00h..01h) destroyed below)
   mov  (x),a        ;= C6         ; (also sets stacktop to 01EFh, kinda
   dec  x            ;= 1D         ; messy, nicer would be stacktop 01FFh)
   bne  .clear       ;= D0 FC
   mov  $F4, #$AA    ;= 8F AA F4   ;\ notify Main CPU that APU is ready
   mov  $F5, #$BB    ;= 8F BB F5   ;/ for communication
.wait:
   cmp  $F4, #$CC    ;= 78 CC F4   ; wait for initial "kick" value
   bne  .wait        ;= D0 FB      ; </
   bra  Start        ;= 2F 19

Block:
   mov  y, $F4       ;= EB F4      ; index (should become 0)
   bne  Block        ;= D0 FC
.data:
   cmp  y, $F4       ;= 7E F4
   bne  .retry       ;= D0 0B
   mov  a, $F5       ;= E4 F5      ; get data
   mov  $F4, y       ;= CB F4      ; ack data
   mov  ($00)+y, a   ;= D7 00      ; store data
   inc  y            ;= FC         ; addr lsb
   bne  .data        ;= D0 F3
   inc  $01          ;= AB 01
.retry:
   bpl  .data        ;= 10 EF
   cmp  y, $F4       ;= 7E F4
   bpl  .data        ;= 10 EB

Start:
   movw ya, $F6      ;= BA F6                  ;\ copy transfer (or entrypoint)
   movw 00, ya       ;= DA 00      ; addr      ;/ address to RAM at (0000h)
   movw ya, $F4      ;= BA F4      ; cmd:kick
   mov  $F4, a       ;= C4 F4      ; ack kick
   mov  a,y          ;= DD         ; cmd
   mov  x,a          ;= 5D         ; cmd
   bne  Block        ;= D0 DB
   jmp  ($0000+x)    ;= 1F 00 00   ; in: A=0, X=0, Y=0, SP=EFh, PSW=02h

dw   RESET           ;= C0 FF     ; reset vector
