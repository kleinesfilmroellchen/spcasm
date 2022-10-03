# Assembler reference

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
- `PSW` (alias: `P`) "program status word" 16-bit flag register. The contained flags are NOP-H-ZC (from high to low bit):
  - `N`: negative flag.
  - `O`: arithmetic overflow flag.
  - `P`: direct page flag, determines whether the direct page is the zero page or the one page.
  - `H`: half carry flag, signals a carry from bit 3 which is a carry situation in binary coded decimal. Is also set on arithmetic overflow.
  - `Z`: zero flag.
  - `C`: carry flag (including shifts).

## Addressing modes

The SPC700 supports various addressing modes, though not all addressing modes are supported for all operands of instructions. The shorthands introduced here will also be used in the full instruction list. Note that in all cases, `$00` is a placeholder for an 8-bit value and `$0000` is a placeholder for a 16-bit value. The cases where these are ambiguous are explained further down with the instructions. The byte count is just the amount of bytes this addressing mode needs as an operand on its own; if there are two operands then the two byte counts need to be added.

| Shorthand       | Alternative syntax(es) | Explanation                                                                                             | # bytes                          | Possible address range |
| --------------- | ---------------------- | ------------------------------------------------------------------------------------------------------- | -------------------------------- | ---------------------- |
| A/X/Y/SP/PSW/YA | P for PSW              | Register                                                                                                | 0                                | Depends on instruction |
| #$00            |                        | 8-bit literal value                                                                                     | 1                                | N/A                    |
| $00             |                        | Data at direct page address                                                                             | 1                                | Direct page            |
| $00+X           |                        | Data at (direct page address + X): X-indexed direct page                                                | 1                                | Direct page            |
| $00+Y           |                        | Y-indexed direct page                                                                                   | 1                                | Direct page            |
| (X)             | [X]                    | Data at the direct page address stored in X: Indirect X                                                 | 0                                | Direct page            |
| (X)+            | [X]+, [X+], (X+)       | Indirect X with auto-increment of X after the instruction executes                                      | 0                                | Direct page            |
| (Y)             | [Y]                    | Indirect Y                                                                                              | 0                                | Direct page            |
| ($00+X)         | [$00+X]                | Indirect X, but the result is used as an address again: X-indexed (double) indirect                     | 1                                | Direct page            |
| ($00)+Y         | [$00]+Y                | Data at direct page address is added to Y, then this is used as an address: Indirect Y-indexed indirect | 1                                | Direct page            |
| $0000           |                        | Data at address                                                                                         | 2                                | All                    |
| $0000+X         |                        | X-indexed (any address)                                                                                 | 2                                | All                    |
| $0000+Y         |                        | Y-indexed (any address)                                                                                 | 2                                | All                    |
| ($0000+X)       |                        | X-indexed indirect (any address)                                                                        | 2                                | All                    |
| $00.bit         |                        | Bit of data at direct page address                                                                      | 1 (bit is baked into the opcode) | Direct page            |
| $0000.bit       |                        | Bit of data (any address)                                                                               | 2 (bit is baked into the opcode) | All                    |
| /$0000.bit      | !$0000.bit             | Negated bit of data                                                                                     | 2 (bit is baked into the opcode) | All                    |
| C               |                        | Carry flag                                                                                              | 0                                | N/A                    |

**This section is incomplete.**

- [ ] Document all mnemonics spcasm recognizes and what they do.
- [x] Document all addressing modes and what they do.
- [ ] Document expression syntax.
- [ ] Document the label system and its limitations.
- [ ] Document all macros and what they do.
