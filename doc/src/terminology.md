# Terminology

spcasm uses some terminology which might be unfamiliar to you if you haven't written much assembly code so far, and which is also used inconsistently across assemblers and tools. This page provides a brief glossary explaining what those terms mean.

### Reference

References are any kind of named placeholder that has a value computed at compile time. References are some of the major ergonomic features of any assembler. These are all references:

```asm
loop: nop           ; global label named "loop"
.case_3: mov a, x   ; local label named "case_3"
mov a, <constant>   ; user macro argument named "constant"
magic = $57         ; global constant named "magic"
```

Other assemblers often call all references labels, as in practice most references are global or local labels. The term "reference" is an attempt of spcasm to clarify what is being discussed and remove confusion. spcasm will (internally and externally) mostly use the term "reference" unless instruction labeling or some kind of label is explicitly (and exclusively) the subject matter.

### Label

Labels are any kind of reference that is used to label an instruction, like so:

```asm
loop:
	nop
	jmp loop   ; notice the use of "loop" as the jump target in this instruction
```

spcasm usually ditches the term "label" in favor of the more general "reference".

Labels are most often used for accessing named data and as jump or branch targets.
