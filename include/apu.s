;;; SNES APU definitions.
;;; This file defines symbols for S-SMP hardware registers, memory regions, etc.
;;; The names are based on the APU manual at
;;; https://problemkaputt.de/fullsnes.htm#snesaudioprocessingunitapu
;;; This file is part of the spcasm SPC-700 assembler.

arch spc700-raw

;;; Memory map.
ZERO_PAGE_START = $0000
IO_START = $00F0
STACK_START = $0100
ONE_PAGE_START = STACK_START
; There's of course RAM before this, but here's where regular code and data should begin.
RAM_START = $0200
BOOTROM_START = $FFC0

;;; I/O ports.
; Undocumented register, should not be touched.
TEST = $00F0
CONTROL = $00F1
; DSP register numbers are defined further down.
DSPADDR = $00F2
DSPDATA = $00F3
; CPU communication ports, equivalent to $2140 - $2143.
CPUIO0 = $00F4
CPUIO1 = $00F5
CPUIO2 = $00F6
CPUIO3 = $00F7
; Aux ports are unconnected but function like general-purpose RAM.
; S-SMP pins 34-27.
AUXIO4 = $00F8
; S-SMP pins 25-18.
AUXIO5 = $00F9
T0DIV = $00FA
T1DIV = $00FB
T2DIV = $00FC
T0OUT = $00FD
T1OUT = $00FE
T2OUT = $00FF

;;; DSP Registers.
;;; These are 8-bit register addresses written to DSPADDR.
;;; $80-$FF are read-only mirrors of $00-$7F.
; Voice-specific registers, should be offset with the voice offset constants below.
VxVOLL = $0
VxVOLR = $1
VxPITCHL = $2
VxPITCHH = $3
VxSRCN = $4
VxADSR1 = $5
VxADSR2 = $6
VxGAIN = $7
VxENVX = $8
VxOUTX = $9
FIRx = $F
; Unused registers; can be used for 8 bytes of general-purpose RAM each.
NAxA = $A
NAxB = $B
NAxE = $E

; Voice offsets; add to registers containing "x" to get the register specific to a voice.
VOICE0 = $00
VOICE1 = $10
VOICE2 = $20
VOICE3 = $30
VOICE4 = $40
VOICE5 = $50
VOICE6 = $60
VOICE7 = $70

; Global DSP registers.
MVOLL = $0C
MVOLR = $1C
EVOLL = $2C
EVOLR = $3C
KON = $4C
KOFF = $5C
FLG = $6C
ENDX = $7C
EFB = $0D
PMON = $2D
NON = $3D
EON = $4D
DIR = $5D
ESA = $6D
EDL = $7D
; Unused register, can be used for general-purpose RAM.
NA_1D = $1D

;;; Writes one byte of data to a DSP register; does not overwrite CPU registers
macro write_dsp(address, data)
	mov.b DSPADDR,#<address>
	mov.b DSPDATA,#<data>
endmacro

;;; Reads one byte of data from a DSP register into the A register.
macro read_dsp(address)
	mov.b DSPADDR,#<address>
	mov.b A,DSPDATA
endmacro
