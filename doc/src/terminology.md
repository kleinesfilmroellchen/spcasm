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

Other assemblers often call all references [labels](#label), as in practice most references are global or local labels. The term "reference" is an attempt of spcasm to clarify what is being discussed and remove confusion. spcasm will (internally and externally) mostly use the term "reference" unless instruction labeling or some kind of label is explicitly (and exclusively) the subject matter.

### Label

Labels are any kind of reference that is used to label an instruction, like so:

```asm
loop:
	nop
	jmp loop   ; notice the use of "loop" as the jump target in this instruction
```

spcasm usually ditches the term "label" in favor of the more general ["reference"](#reference).

Labels are most often used for accessing named data and as jump or branch targets.

### Directive

An assembler directive, sometimes confusingly called an assembler macro (see [Macro](#macro)), is any command to the assembler that is not simply an instruction or a pseudoinstruction. Sometimes in other assemblers, other categories of "commands" exist, but spcasm makes no further distinction. Directives generally perform higher-level functionality that is not accessible via instructions alone. In modern assemblers, directives are usually preceded with a `.` (or some other special symbol), but spcasm inherits the vasm oldstyle syntax and generally mimics old assemblers. Therefore, there is no distinctive `.` to start a directive invocation. Some examples for directives:

```asm
byte 3, 4, 5          ; assembles a "table" of literal bytes
org $8000           ; switches assembler memory location to hex 8000
macro my_user_macro ; starts a user-defined macro which, if not called, will not appear in the assembly output
endmacro            ; ends the macro currently being defined
%my_user_macro()    ; calls the macro named "my_user_macro"; the directive keyword is "%" if you want to be pedantic
```

As you can see, the definition and use of [user-defined macros](#macro) is controlled via directives.

### Macro

Also referred to as **user macro** or **user-defined macro** in code and output of spcasm. Macros are defined by the user, and are mostly a fancy assembly code copy-paste mechanism:

```asm
; This code...:
macro tax(addition)
	mov a, x
	adc a, addition
endmacro

	%tax(34)
	nop

; ... is the same as this code:
	mov a, x
	adc a, 34
	nop

```

Some assemblers generally refer to non-instruction "commands" (or program elements) as "macros", which (like the label <-> reference situation) leads to unnecessary confusion. spcasm only calls user-defined macros such as the above "macros", and everything else is called a [directive](#directive).
