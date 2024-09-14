# Assembler reference

- auto-gen TOC;
  {:toc}

This section strives to be a developer reference for the SPC700 instruction set. It is based on a couple of other documents: mainly a [very comprehensive SNES APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt), [Gau's higher-level overview](http://emureview.ztnet.com/developerscorner/SoundCPU/spc.htm), the [nocash SNES hardware specification](https://problemkaputt.de/fullsnes.htm#snesapudspbrrsamples), and the [Asar manual](https://rpghacker.github.io/asar/manual/). Note that because spcasm mostly concerns itself with SPC700 programming, I/O aspects are barely documented here; the nocash specification and the APU manual have very detailed information on how the DSP works and how it can be utilized.

In the following, memory addresses are always hexadecimal.

## Basic architectural overview

The SPC700 has access to 64KB of memory, which contains RAM, ROM and memory-mapped I/O. The memory is mapped as follows:

- 0000-00EF: Zero page RAM
- 00F0-00FF: I/O (and zero page)
- 0100-01FF: One page RAM and fixed location of CPU hardware stack
- 0200-FFBF: RAM
- FFC0-FFFF: boot ROM or RAM (selected with CONTROL hardware register at 00F1)

For programming, this mainly means:

- Code and data should start no earlier than 0200.
- Common variables can live in the zero page except F0-FF.
- The one page may be used for common variables, but care needs to be taken that they don't collide with the stack. The stack grows from 01FF downwards, so if it doesn't get too large, the lower portion of the one page can be used.
- The boot ROM at FFC0 should not be active during normal operation; deselect it after the initial data transfer sequence. Then this memory is usable as normal.

Some instructions can only address certain memory locations; this is documented further below.

The SPC700 has six registers, two of which can be paired into an additional 16-bit register:

- `A` 8-bit general-purpose register.
- `X` 8-bit general-purpose (index) register.
- `Y` 8-bit general-purpose (index) register.
- `YA` Pairing of Y (high bits) and A (low bits) into a 16-bit register.
- `SP` 16-bit stack pointer register; upper 8 bits are fixed to 01 meaning the possible range of values is 0100-01FF.
- `PC` 16-bit program counter or instruction pointer register.
- `PSW` (alias: `P`) "program status word" 8-bit flag register. The contained flags are NVPBHIZC (from high to low bit). Note that B and I are not used much in the S-SMP and some manuals indicate that they are initialized and/or frequently overwritten with garbage.
  - `N`: negative flag.
  - `V`: arithmetic overflow flag.
  - `P`: direct page flag, determines whether the direct page is the zero page or the one page.
  - `B`: break flag. The purpose of this flag is unknown and because BRK is not useful on the S-SMP, it can be disregarded.
  - `H`: half carry flag, signals a carry from bit 3 which is a carry situation in binary coded decimal. Is also set on arithmetic overflow.
  - `I`: Interrupt flag. The purpose of this flag is unknown, but since the S-SMP has no interrupt sources, it can be disregarded.
  - `Z`: zero flag.
  - `C`: carry flag (including shifts).

## Addressing modes

The SPC700 supports various addressing modes, though not all addressing modes are supported for all operands of instructions. The shorthands introduced here will also be used in the full instruction list. Note that in all cases, `$00` is a placeholder for an 8-bit value and `$0000` is a placeholder for a 16-bit value. The cases where these are ambiguous are explained further down with the instructions. The byte count is just the amount of bytes this addressing mode needs as an operand on its own; if there are two operands then the two byte counts need to be added.

| Shorthand       | Alternative syntax(es) | Explanation                                                                                             | # bytes                       | Possible address range |
| --------------- | ---------------------- | ------------------------------------------------------------------------------------------------------- | ----------------------------- | ---------------------- |
| A/X/Y/SP/PSW/YA | P for PSW              | Register                                                                                                | 0                             | Depends on instruction |
| #$00            |                        | 8-bit literal value                                                                                     | 1                             | N/A                    |
| $00             |                        | Data at direct page address                                                                             | 1                             | Direct page            |
| $00+X           |                        | Data at (direct page address + X): X-indexed direct page                                                | 1                             | Direct page            |
| $00+Y           |                        | Y-indexed direct page                                                                                   | 1                             | Direct page            |
| (X)             | [X]                    | Data at the direct page address stored in X: Indirect X                                                 | 0                             | Direct page            |
| (X)+            | [X]+, [X+], (X+)       | Indirect X with auto-increment of X after the instruction executes                                      | 0                             | Direct page            |
| (Y)             | [Y]                    | Indirect Y                                                                                              | 0                             | Direct page            |
| ($00+X)         | [$00+X]                | Indirect X, but the result is used as an address again: X-indexed (double) indirect                     | 1                             | Direct page            |
| ($00)+Y         | [$00]+Y                | Data at direct page address is added to Y, then this is used as an address: Indirect Y-indexed indirect | 1                             | Direct page            |
| $0000           |                        | Data at address                                                                                         | 2                             | All                    |
| $0000+X         |                        | X-indexed (any address)                                                                                 | 2                             | All                    |
| $0000+Y         |                        | Y-indexed (any address)                                                                                 | 2                             | All                    |
| ($0000+X)       |                        | X-indexed indirect (any address)                                                                        | 2                             | All                    |
| $00.bit         |                        | Bit of data at direct page address                                                                      | 1 (bit is baked into opcode)  | Direct page            |
| $0000.bit       |                        | Bit of data (any address)                                                                               | 2 (bit is baked into address) | Up to 1FFF inclusive   |
| /$0000.bit      | !$0000.bit             | Negated bit of data                                                                                     | 2 (bit is baked into address) | Up to 1FFF inclusive   |
| n               |                        | index into $FFC0 call table                                                                             | 0 (baked into opcode)         | 4 bits (0-15)          |
| C               |                        | Carry flag                                                                                              | 0                             | N/A                    |

Apart from the addressable memory constraints outlined above, some instructions operate on memory locations which have some of their memory address fixed by hardware. This is documented with these specific instructions.

### Addressing mode suffixes

Several instructions can operate both on the direct page only as well as most of the address space. spcasm includes a direct page optimizer which automatically uses the direct page version of the instruction wherever possible. If this doesn't work, you can use the `.b` suffix on all applicable instructions to force the direct page version of the instruction. If, however, the larger version of the instruction is desired instead, you can use the `.w` suffix instead.

## Instruction encoding details

To program the SPC700, understanding what an addressing mode does and where it can be used is enough. This section will explain the surprising intricacies and complexities of encoding SPC700 instructions. This is the explanation I would have liked to have when I started creating spcasm, I hope it is helpful to anyone hand-verifying assembly, testing assemblers or disassembling SPC700 code.

### Operand order

The operand order in the assembler is target, source by convention. However, in machine code, the normal order is `opcode source target`, which is exactly reversed. This is not relevant often, as many instructions either only take one operand, or they take at least one operand that is encoded as part of the opcode, such as a register. _However_, the two-operand versions of the `BBS`, `BBC`, `CBNE`, and `DBNZ` instructions (apart from also partially being bit indexed instructions) take both a direct page address and a relative jump target. Here, the direct page address, the "target" in assembler syntax, is not the second, but the first machine operand. To give an example: **While `OR target, source` is encoded as `opcode source target`, `BBS dp.bit, rel` is encoded as `opcode dp rel`.** As far as I can tell, no other assembly reference documents this encoding "feature" properly.

### Relative addresses

All branch instructions take a relative address, as they are intended to jump forward or backward only a small amount and can therefore afford to only store one byte of signed offset to allow jumps by +127 or -128. Note that the jump is relative to the instruction _following the branch instruction_, as the program counter will already have incremented by the time the branch is taken. Therefore, for example, to unconditionally branch to the exact same instruction again (the most memory-efficient way of writing an infinite loop) the offset needs to be -2, or FE in hexadecimal. A branch offset of 0 will have no effect on instruction flow, as it points to the instruction after the branch.

In practice, spcasm will automatically compute the correct offset necessary to perform a branch. You have to specify the _target address_ as usual, not the offset. Im most cases the target will be a label to branch to. Although it should not be necessary, with this knowledge, you can use assembly-time calculations to revert the relative computations and specify an exact offset.

### Endianness and 16-bit instructions

The SPC700 is a little endian architecture, meaning that the least-significant byte comes first in memory. This is not only relevant for instruction encoding of memory addresses which the user does not need to worry about, but also the behavior of the 16-bit instructions. Some of these instructions contain the suffix "W" for "wide", but plenty of control flow instructions also need to consider whole words of memory. Whenever they operate on the direct page, the specified memory address forms the lower byte, and the memory address after this forms the higher byte. The second address wraps around at page boundaries, so if the first address has the lower byte $FF, the second address has the lower byte $00 but the same upper byte! For instance, when accessing a direct page pointer through double-indirect addressing `($FF+X)`, X is zero and the P flag is 1, this would read the pointer's low byte from $01FF and the high byte from $0100. In the pseudo-register YA, the A register forms the lower byte.

The little endianness is also preserved for stack operations, most importantly all call instructions, which push the 16-bit program counter. This means they first push the higher 8 bits of the program counter to the stack, and then the lower 8 bits, and since the stack grows downwards, this yields a little-endian address. That simplifies any operations which manually retrieve the return address.

### Bit indexing

Some instructions provide bit indexing, where a single bit of a memory address is inspected or operated on. There are two different classes of bit indexing used by different instructions:

- Direct page bit indexing: Used by `BBS`, `BBC`, `SET1`, `CLR1`, which operate on or inspect bits in the direct page. They encode the bit index within the opcode itself, so while the mnemonic and the bit index are usually written separately from each other, they are combined into the opcode byte and there are eight different opcodes for each of the mnemonics. Specifically, the bit index occupies the upper three bits of the opcode byte, and the lower five bits are determined by the mnemonic and can be found in the instruction reference.
- General address bit indexing: Used by the other bit instructions `AND1`, `OR1`, `EOR1`, `NOT1`, `MOV1`. Here, the bit index is encoded in the upper three bits of the address itself. This is the reason that these instructions can only access memory up to 1FFF inclusive. Remember that because the SPC700 is little endian, the bit index is found in the third byte of the machine instruction, because the first byte is the opcode and the second byte is the low byte of the address.

For Asar compatibility, you may omit the bit index, which then behaves like an implicit bit index of 1 like it would in Asar. This is not a recommended feature to use.

### `TCALL` encoding

The `TCALL` instruction is similar to a direct page bit indexing instruction in that it encodes its parameter, the table index, into the opcode. However, the table index can range from 0 to 15, not just from 0 to 7. Therefore, `TCALL` uses the entire high nybble to store the table index.

## Values and expressions

In spcasm, it is possible to run math at assembly time, which is especially useful for reference-dependent calculations. Math expressions can be used in place of most numbers; in fact numeric literal syntax will also be discussed here.

Some values need to be resolvable early on or have additional restrictions. For example, many directives do not accept references in their values, as this can create unsolvable reference resolution problems. spcasm will never silently accept forbidden values in these cases, so you will always get an error and can look up the specifics in the [error reference](../errors.md).

Note that while numbers can be larger than 8 or 16 bits, even if just in intermediary steps (e.g. before a division), spcasm internally uses 64-bit integers and depending on your system sometimes also 32-bit integers later on.

Numeric literals are given in decimal by default. The prefix for hexadecimal literals is `$`, the prefix for binary literals is `%`.

```asm
; These are all equivalent:
byte 40
byte $28
byte %101000
```

Math expression syntax is similar to C-like programming languages. Note that as opposed to Asar, **spcasm does not have a legacy priority mode. The `math pri` directive is detected and causes an error**. All spcasm expressions follow mathematical order of operations and all operations except for exponentiation are left-associative. The following list of available operations is sorted by priority, with the least strongly binding operation first.

- conditional operations, which return 1 for true and 0 for false:
  - `==` Numeric equality
  - `!=` Numeric inequality
  - `>=` Numerically greater or equals
  - `<=` Numerically less or equals
  - `>` Numerically greater
  - `<` Numerically less
- `|` Logical bitwise or
- `&` Logical bitwise and
- `^` Logical bitwise exclusive or
- `<<` Logical left shift, `>>` Arithmetic (sign-extending) right shift
- `+` Addition, `-` subtraction
- `*` multiplication, `/` integer division, `%` modulus (remainder of integer division)
- `**` exponentiation
- `+` Unary plus without effect, `-` unary negation, `~` logical not (all bits inverted)
- `()` parenthesizing sub-expressions
- a literal number
- a reference or a macro parameter

```asm
; Results are given in hex:
80 / 10         ; 08
+(80 / 10) + 5  ; 0D
80 / 10 + 5     ; 0D
80 / (10 + 5)   ; 05
+(80 / 10) +X   ; This is also an x-indexed addressing mode despite the '+'
40 - 30 - 10    ; 00
$10 * 3         ; 30
72 % 7          ; 02
1 << 4          ; 10
8 >> 2          ; 02
$ff & %11       ; 03
$0F | $f0       ; FF
~%10110010      ; 4D
$9f ^ $78 ^ $9F ; 78
6 ** 3          ; D8
5 == 4          ; 00 (false)
5 != 4          ; 01 (true)
8 == 4+4        ; 01
7 < 10          ; 01
6 >= 19 * 2     ; 00
```

Note that using parentheses might in some instances confuse spcasm over whether you are trying to use an indirect addressing mode with an instruction, or whether you are trying to create a math expression. To force anything into indirect addressing, use square brackets `[]` instead of round parentheses. To force anything into a math expression, use round parentheses and a prefixed `+` unary operation that is a no-op and mainly exists for exactly this purpose.

Depending on what the number is to be used for, it may be truncated to 8 bits, 16 bits, or treated as an address and used to compute a relative offset.

For the purposes of boolean expressions, such as assembly-time conditionals, all values except 0 are treated as true and 0 itself is treated as false.

## Labels and references

Especially for control flow, but also for naming memory locations or defining constants, spcasm has a rich reference system. (See [Terminology](../terminology.md) if you want to know what difference there is between labels and references.)

To label any memory location, use the name of the label followed by a colon:

```asm
entry:

  mov a, #7
  mov x, 3

important_data: db 0

important_word: word $8076
```

Currently you cannot have two labels pointing to the same memory location with this syntax, but it is possible to assign one label the value of another label (see below).

The label created in the above example is global. Such labels are available everywhere and across files, macro calls, etc. To avoid name collisions with common label names such as `loop`, `end`, `error` etc., you can use local labels by preceding the label name with a dot `.`. Local labels are only visible between their parent global label (the previous global label) and the next global label. Therefore, there must be a global label defined before you can define the first local label.

```asm
fill_with_a:
    mov y, 10
  .loop:
    mov $3040+y, a
    dbnz y, .loop
    mov a, #0
    ret

fill_with_x:
    push a
    mov a, x
    mov y, 10
  .loop:             ; This does not collide with the first .loop above
    mov $3040+y, a
    dbnz y, .loop
    pop a
    ret
```

You can also define more deeply nested local labels with `..twice_nested`, `...thrice_nested`, etc. Their parent label is the most recent label with a higher level, so they do not need to be nested precisely:

```asm
global:

.once_local ; Child of global

...thrice_nested ; Child of once_local

..twice_nested ; Child of once_local

...thrice_nested ; Child of twice_nested
```

It is also possible to define a reference to a fixed value, or one computed from other references, etc. For this, a simple assignment syntax is supported:

```asm
; In general:
reference = expression

; Examples:
entry_2 = entry_1           ; Make two code labels coincide
important_variable = $50ff
loop_cycles = $32 >> 7
```

These labels always have to be global; local labels are specifically intended for code labeling and jump targets.

When assigning references in this way, take care to not create a cyclic dependency. Such dependencies will be caught by spcasm in form of an unresolved reference error after the reference resolution limit was reached.

### Pseudo references

It is also possible to reference local references of any level with what spcasm calls "pseudo references". With pseudo references, you can refer to child references of other references by simply concatenating the reference names with underscores, like so:

```asm

start:
  ...

  .jumptable:
  ..mode_1: db 10
  ..mode_2: db 20

; Can't refer to the jump table entries with normal references after this!
other_subroutine:
  jmp (start_jumptable+X)
  ; Or even deeper nesting:
  jmp start_jumptable_mode_2
```

Note that pseudo references are a brittle Asar compatibility feature and their use is discouraged. spcasm will **always** first resolve references normally. This means that if a global reference has the same name as a local reference when referred to by a pseudo label name, the global reference will always be used instead, and spcasm will not inform you of this fact.

```asm

some_global:
  .function:

some_global_function:
  ...

other_function:
  ; Resolves to "some_global_function" instead of the local ".function" under "some_global"!
  jmp some_global_function

```

### Asar defines

spcasm supports Asar's define directives:

```asm
!SOME_DEFINE = 3
!SOME_OTHER_DEFINE = 5
```

Note that since these are almost exclusively used for constants, spcasm treats them like any other label.
