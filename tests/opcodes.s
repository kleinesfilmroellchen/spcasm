arch spc700-raw
org 0
startpos
start:        ; @ 0
   MOV A,#$10    ;= E8 10
   MOV A,(X)     ;= E6
   MOV A,(X+)    ;= BF
   MOV A,$32     ;= E4 32
   MOV A,$A1+X   ;= F4 A1
   MOV A,$4502   ;= E5 02 45
   MOV A,$3320+X ;= F5 20 33
   MOV A,$FF00+Y ;= F6 00 FF
   MOV A,($40+X) ;= E7 40
data:         ; @ 13
   MOV A,(%00100000)+Y ;= F7 20
   MOV X,#$60    ;= CD 60
   MOV X,$9A     ;= F8 9A
   MOV X,$2E+Y   ;= F9 2E
   MOV X,$2F00   ;= E9 00 2F
   MOV Y,#$0F    ;= 8D 0F
   MOV Y,$48     ;= EB 48
   MOV Y,%01+X   ;= FB 01
   MOV Y,$2100   ;= EC 00 21

   MOV A,#data     ;= E8 13
   MOV A,data      ;= E4 13
   MOV A,data+X    ;= F4 13
   MOV A,data+Y    ;= F6 13 00
   MOV A,(data+X)  ;= E7 13
   MOV A,(data)+Y  ;= F7 13
   MOV X,#data     ;= CD 13
   MOV X,data      ;= F8 13
   MOV Y,#data     ;= 8D 13
   MOV Y,data      ;= EB 13

   MOV (X),A       ;= C6
   MOV (X+),A      ;= AF
   MOV $50,A       ;= C4 50
   MOV $11+X,A     ;= D4 11
   MOV $2290,A     ;= C5 90 22
   MOV $2294+X,A   ;= D5 94 22
   MOV $2295+Y,A   ;= D6 95 22
   MOV ($98+X),A   ;= C7 98
   MOV ($F7)+Y,A   ;= D7 F7
   MOV $1E,X       ;= D8 1E
   MOV $17+Y,X     ;= D9 17
   MOV $1E00,X     ;= C9 00 1E
   MOV $1A,Y       ;= CB 1A
   MOV $18+X,Y     ;= DB 18
   MOV $1F00,Y     ;= CC 00 1F

   MOV A,X         ;= 7D
   MOV A,Y         ;= DD
   MOV X,A         ;= 5D
   MOV Y,A         ;= FD
   MOV X,SP        ;= 9D
   MOV SP,X        ;= BD
   MOV $A3,$2D     ;= FA 2D A3
   MOV $88,#%1001  ;= 8F 09 88

   adc A,#%100     ;= 88 04
   adc A,(X)       ;= 86
   adc A,$60       ;= 84 60
   adc A,$61+X     ;= 94 61
   adc A,$8086     ;= 85 86 80
   adc A,$8087+X   ;= 95 87 80
   adc A,$8088+Y   ;= 96 88 80
   adc A,($62+X)   ;= 87 62
   adc A,($63)+Y   ;= 97 63
   adc (X),(Y)     ;= 99
   adc $70,$71     ;= 89 71 70
   adc $73,#$0D    ;= 98 0D 73

   sbc A,#%100  ;= A8 04
   sbc A,(X)  ;= A6
   sbc A,$60  ;= A4 60
   sbc A,$61+X  ;= B4 61
   sbc A,$8086  ;= A5 86 80
   sbc A,$8087+X  ;= B5 87 80
   sbc A,$8088+Y  ;= B6 88 80
   sbc A,($62+X)  ;= A7 62
   sbc A,($63)+Y  ;= B7 63
   sbc (X),(Y)  ;= B9
   sbc $70,$71  ;= A9 71 70
   sbc $73,#$0D  ;= B8 0D 73

   and A,#%100  ;= 28 04
   and A,(X)  ;= 26
   and A,$60  ;= 24 60
   and A,$61+X  ;= 34 61
   and A,$8086  ;= 25 86 80
   and A,$8087+X  ;= 35 87 80
   and A,$8088+Y  ;= 36 88 80
   and A,($62+X)   ;= 27 62
   and A,($63)+Y   ;= 37 63
   and (X),(Y)  ;= 39
   and $70,$71  ;= 29 71 70
   and $73,#$0D  ;= 38 0D 73

   or A,#%100   ;= 08 04
   or A,(X)   ;= 06
   or A,$60   ;= 04 60
   or A,$61+X   ;= 14 61
   or A,$8086   ;= 05 86 80
   or A,$8087+X   ;= 15 87 80
   or A,$8088+Y   ;= 16 88 80
   or A,($62+X)   ;= 07 62
   or A,($63)+Y   ;= 17 63
   or (X),(Y)   ;= 19
   or $70,$71   ;= 09 71 70
   or $73,#$0D   ;= 18 0D 73

   eor A,#%100   ;= 48 04
   eor A,(X)   ;= 46
   eor A,$60   ;= 44 60
   eor A,$61+X   ;= 54 61
   eor A,$8086   ;= 45 86 80
   eor A,$8087+X   ;= 55 87 80
   eor A,$8088+Y   ;= 56 88 80
   eor A,($62+X)   ;= 47 62
   eor A,($63)+Y   ;= 57 63
   eor (X),(Y)   ;= 59
   eor $70,$71   ;= 49 71 70
   eor $73,#$0D   ;= 58 0D 73

   cmp A,#%100   ;= 68 04
   cmp A,(X)   ;= 66
   cmp A,$60   ;= 64 60
   cmp A,$61+X   ;= 74 61
   cmp A,$8086   ;= 65 86 80
   cmp A,$8087+X   ;= 75 87 80
   cmp A,$8088+Y   ;= 76 88 80
   cmp A,($62+X)   ;= 67 62
   cmp A,($63)+Y   ;= 77 63
   cmp (X),(Y)   ;= 79
   cmp $70,$71   ;= 69 71 70
   cmp $73,#$0D   ;= 78 0D 73
   cmp X,#$76     ;= C8 76
   cmp X,$74      ;= 3E 74
   cmp X,$7500    ;= 1E 00 75
   cmp Y,#$76     ;= AD 76
   cmp Y,$74      ;= 7E 74
   cmp Y,$7500    ;= 5E 00 75

   inc A      ;= BC
   inc $01    ;= AB 01
   inc $02+X  ;= BB 02
   inc $0304  ;= AC 04 03
   inc X      ;= 3D
   inc Y      ;= FC

   dec A      ;= 9C
   dec $01    ;= 8B 01
   dec $02+X  ;= 9B 02
   dec $0304  ;= 8C 04 03
   dec X      ;= 1D
   dec Y      ;= DC

   asl A     ;= 1C
   asl $05   ;= 0B 05
   asl $06+X ;= 1B 06
   ASL $0708 ;= 0C 08 07

   LSR A     ;= 5C
   LSR $05   ;= 4B 05
   LSR $06+X ;= 5B 06
   LSR $0708 ;= 4C 08 07

   rol A     ;= 3C
   rol $05   ;= 2B 05
   rol $06+X ;= 3B 06
   rol $0708 ;= 2C 08 07

   ror A     ;= 7C
   ror $05   ;= 6B 05
   ror $06+X ;= 7B 06
   ror $0708 ;= 6C 08 07

   xcn A     ;= 9F

   MOVW YA,$09 ;= BA 09
   MOVW $0A,YA ;= DA 0A
   INCW $0B    ;= 3A 0B
   DECW $0C    ;= 1A 0C
   ADDW YA,$0D ;= 7A 0D
   SUBW YA,$0E ;= 9A 0E
   CMPW YA,$0F ;= 5A 0F

   MUL YA   ;= CF
   DIV YA,X ;= 9E

reltarget:    ; @ 01 62
   daa A ;= DF
.subtarget:   ; @ 01 63
   das A ;= BE

   bra $100       ;= 2F 9E
               ; label location is -4
   beq reltarget    ;= F0 FA
   bne reltarget    ;= D0 F8
   bcs reltarget+2  ;= B0 F8
   bcc reltarget+4  ;= 90 F8
   bvs reltarget+6  ;= 70 F8
   bvc reltarget+8  ;= 50 F8
   bmi reltarget+10 ;= 30 F8
   bpl reltarget+12 ;= 10 F8
   cbne $2B,$2C  ;= 2E 2B B7
   cbne $2D+X,$2E;= DE 2D B6
   dbnz $2F,$30  ;= 6E 2F B5
   dbnz Y,$31    ;= FE B4
   jmp $2122     ;= 5F 22 21
   jmp ($2324+X)     ;= 1F 24 23

   jmp .subtarget  ;= 5F 5F 01
   jmp reltarget   ;= 5F 5E 01
another_global: ; @ 01 8D
   NOP             ;= 00
.subtarget:     ; @ 01 8E
   NOP             ;= 00
   jmp .subtarget  ;= 5F 8A 01

.selftarget:
   bne .selftarget       ;= D0 FE
global_selftarget:
   bne global_selftarget ;= D0 FE

   bbs data.0,$F9 ;= 03 13 64
   bbs $30.1,$F9 ;= 23 30 61
   bbs $30.2,$F9 ;= 43 30 5E
   bbs $30.3,$F9 ;= 63 30 5B
   bbs $30.4,$F9 ;= 83 30 58
   bbs $30.5,$F9 ;= A3 30 55
   bbs $30.6,$F9 ;= C3 30 52
   bbs $30.7,$F9 ;= E3 30 4F
   bbc start.0,$05 ;= 13 00 58
   bbc $31.1,$05 ;= 33 31 55
   bbc $31.2,$05 ;= 53 31 52
   bbc $31.3,$05 ;= 73 31 4F
   bbc $31.4,$05 ;= 93 31 4C
   bbc $31.5,$05 ;= B3 31 49
   bbc $31.6,$05 ;= D3 31 46
   bbc $31.7,$05 ;= F3 31 43

   call $5060 ;= 3F 60 50
   pcall $02  ;= 4F 02
   tcall $5   ;= 51
   TCALL $8   ;= 81
   tcall $F   ;= F1
   BRK        ;= 0F
   ret        ;= 6F
   reti       ;= 7F

   push A    ;= 2D
   push X    ;= 4D
   push Y    ;= 6D
   push P  ;= 0D

   pop A    ;= AE
   pop X    ;= CE
   pop Y    ;= EE
   pop P  ;= 8E

   set $50.5  ;= A2 50
   clr $51.7  ;= F2 51

   tset $5060,a ;= 0E 60 50
   tclr $1011,a ;= 4E 11 10

   and1 C,$1010.0  ;= 4A 10 10 
   and1 C,!$1020.3 ;= 6A 20 70
   or1 C,$1030.7   ;= 0A 30 F0
   or1 C,!$1040.6  ;= 2A 40 D0
   eor1 C,$1070.2  ;= 8A 70 50
   not1 $1080.5    ;= EA 80 B0
   mov1 C,$1090.4  ;= AA 90 90
   mov1 $10A0.1,C  ;= CA A0 30

   clrc  ;= 60
   setc  ;= 80
   notc  ;= ED
   clrv  ;= E0
   clrp  ;= 20
   setp  ;= 40
   ei  ;= A0
   di  ;= C0

   nop  ;= 00
   sleep  ;= EF
   stop  ;= FF
