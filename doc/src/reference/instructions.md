# Instructions

All SPC700 instructions have the form `MNEMONIC TARGET,SOURCE`. The addressable memory range and flags set depend on the exact opcode. When flags are not given, the instruction does not set any flags but might inspect them.

All opcodes are given in hexadecimal; other numbers are decimal.

## `MOV`

`MOV` instructions move data from the source operand into the target operand.

| Operands   | Opcode | Total bytes | Cycles | Flags set |
| ---------- | ------ | ----------- | ------ | --------- |
| A, #$00    | E8     | 2           | 2      | NZ        |
| A, (X)     | E6     | 1           | 3      | NZ        |
| A, (X)+    | BF     | 1           | 4      | NZ        |
| A, $00     | E4     | 2           | 3      | NZ        |
| A, $00+X   | F4     | 2           | 4      | NZ        |
| A, $0000   | E5     | 3           | 4      | NZ        |
| A, $0000+X | F5     | 3           | 5      | NZ        |
| A, $0000+Y | F6     | 3           | 5      | NZ        |
| A, ($00+X) | E7     | 2           | 6      | NZ        |
| A, ($00)+Y | F7     | 2           | 6      | NZ        |
| X, #$00    | CD     | 2           | 2      | NZ        |
| X, $00     | F8     | 2           | 3      | NZ        |
| X, $00+Y   | F9     | 2           | 4      | NZ        |
| X, $0000   | E9     | 3           | 4      | NZ        |
| Y, #$00    | 8D     | 2           | 2      | NZ        |
| Y, $00     | EB     | 2           | 3      | NZ        |
| Y, $00+X   | FB     | 2           | 4      | NZ        |
| Y, $0000   | EC     | 3           | 4      | NZ        |
| (X), A     | C6     | 1           | 4      |           |
| (X)+, A    | AF     | 1           | 4      |           |
| $00, A     | C4     | 2           | 4      |           |
| $00+X, A   | D4     | 2           | 5      |           |
| $0000, A   | C5     | 3           | 5      |           |
| $0000+X, A | D5     | 3           | 6      |           |
| $0000+Y, A | D6     | 3           | 6      |           |
| ($00+X), A | C7     | 2           | 7      |           |
| ($00)+Y, A | D7     | 2           | 7      |           |
| $00, X     | D8     | 2           | 4      |           |
| $00+Y, X   | D9     | 2           | 5      |           |
| $0000, X   | C9     | 3           | 5      |           |
| $00, Y     | CB     | 2           | 4      |           |
| $00+X, Y   | DB     | 2           | 5      |           |
| $0000, Y   | CC     | 3           | 5      |           |
| A, X       | 7D     | 1           | 2      | NZ        |
| A, Y       | DD     | 1           | 2      | NZ        |
| X, A       | 5D     | 1           | 2      | NZ        |
| Y, A       | FD     | 1           | 2      | NZ        |
| X, SP      | 9D     | 1           | 2      | NZ        |
| SP, X      | BD     | 1           | 2      |           |
| $00, $00   | FA     | 3           | 5      |           |
| $00, #$00  | 8F     | 3           | 5      |           |

## `ADC`

`ADC` instructions add the source to the target operand, then move the result into the target operand. They also add the carry as a "+1" into this, therefore they can be chained into multi-byte additions.

| Operands  | Opcode | Total bytes | Cycles | Flags set |
| --------- | ------ | ----------- | ------ | --------- |
| A,#$00    | 88     | 2           | 2      | NVHZC     |
| A,(X)     | 86     | 1           | 3      | NVHZC     |
| A,$00     | 84     | 2           | 3      | NVHZC     |
| A,$00+X   | 94     | 2           | 4      | NVHZC     |
| A,$0000   | 85     | 3           | 4      | NVHZC     |
| A,$0000+X | 95     | 3           | 5      | NVHZC     |
| A,$0000+Y | 96     | 3           | 5      | NVHZC     |
| A,($00+X) | 87     | 2           | 6      | NVHZC     |
| A,($00)+Y | 97     | 2           | 6      | NVHZC     |
| (X),(Y)   | 99     | 1           | 5      | NVHZC     |
| $00,$00   | 89     | 3           | 6      | NVHZC     |
| $00,#$00  | 98     | 3           | 5      | NVHZC     |

## `SBC`

`SBC` instructions subtract the target from the source, then move the result into the target operand. They also subtract the carry as a "-1" from this, therefore they can be chained into multi-byte subtractions.

| Operands  | Opcode | Total bytes | Cycles | Flags set |
| --------- | ------ | ----------- | ------ | --------- |
| A,#$00    | A8     | 2           | 2      | NVHZC     |
| A,(X)     | A6     | 1           | 3      | NVHZC     |
| A,$00     | A4     | 2           | 3      | NVHZC     |
| A,$00+X   | B4     | 2           | 4      | NVHZC     |
| A,$0000   | A5     | 3           | 4      | NVHZC     |
| A,$0000+X | B5     | 3           | 5      | NVHZC     |
| A,$0000+Y | B6     | 3           | 5      | NVHZC     |
| A,($00+X) | A7     | 2           | 6      | NVHZC     |
| A,($00)+Y | B7     | 2           | 6      | NVHZC     |
| (X),(Y)   | B9     | 1           | 5      | NVHZC     |
| $00,$00   | A9     | 3           | 6      | NVHZC     |
| $00,#$00  | B8     | 3           | 5      | NVHZC     |

## `CMP`

`CMP` instructions perform a subtraction of target-source similar to `SBC` (except without the carry), then set the flags depending on the result of this subtraction. The result is not stored anywhere. As the name implies, this instruction can be used to numerically compare values:

| Relation        | N   | Z   | C   |
| --------------- | --- | --- | --- |
| source > target | -   | 0   | 0   |
| source ≥ target | -   | -   | 0   |
| source = target | -   | 1   | -   |
| source ≠ target | -   | 0   | -   |
| source ≤ target | 1   | 1   | 1   |
| source < target | 1   | -   | 1   |

| Operands   | Opcode | Total bytes | Cycles | Flags set |
| ---------- | ------ | ----------- | ------ | --------- |
| A, #$00    | 68     | 2           | 2      | NZC       |
| A, (X)     | 66     | 1           | 3      | NZC       |
| A, $00     | 64     | 2           | 3      | NZC       |
| A, $00+X   | 74     | 2           | 4      | NZC       |
| A, $0000   | 65     | 3           | 4      | NZC       |
| A, $0000+X | 75     | 3           | 5      | NZC       |
| A, $0000+Y | 76     | 3           | 5      | NZC       |
| A, ($00+X) | 67     | 2           | 6      | NZC       |
| A, ($00)+Y | 77     | 2           | 6      | NZC       |
| (X), (Y)   | 79     | 1           | 5      | NZC       |
| $00, $00   | 69     | 3           | 6      | NZC       |
| $00, #$00  | 78     | 3           | 5      | NZC       |
| X, #$00    | C8     | 2           | 2      | NZC       |
| X, $00     | 3E     | 2           | 3      | NZC       |
| X, $0000   | 1E     | 3           | 4      | NZC       |
| Y, #$00    | AD     | 2           | 2      | NZC       |
| Y, $00     | 7E     | 2           | 3      | NZC       |
| Y, $0000   | 5E     | 3           | 4      | NZC       |

## `AND`

`AND` instructions perform logical bitwise AND on the operands, then store the result in the target operand.

| Operands   | Opcode | Total bytes | Cycles | Flags set |
| ---------- | ------ | ----------- | ------ | --------- |
| A, #$00    | 28     | 2           | 2      | NZ        |
| A, (X)     | 26     | 1           | 3      | NZ        |
| A, $00     | 24     | 2           | 3      | NZ        |
| A, $00+X   | 34     | 2           | 4      | NZ        |
| A, $0000   | 25     | 3           | 4      | NZ        |
| A, $0000+X | 35     | 3           | 5      | NZ        |
| A, $0000+Y | 36     | 3           | 5      | NZ        |
| A, ($00+X) | 27     | 2           | 6      | NZ        |
| A, ($00)+Y | 37     | 2           | 6      | NZ        |
| (X), (Y)   | 39     | 1           | 5      | NZ        |
| $00, $00   | 29     | 3           | 6      | NZ        |
| $00, #$00  | 38     | 3           | 5      | NZ        |

## `OR`

`OR` instructions perform logical bitwise OR on the operands, then store the result in the target operand.

| Operands   | Opcode | Total bytes | Cycles | Flags set |
| ---------- | ------ | ----------- | ------ | --------- |
| A, #$00    | 08     | 2           | 2      | NZ        |
| A, (X)     | 06     | 1           | 3      | NZ        |
| A, $00     | 04     | 2           | 3      | NZ        |
| A, $00+X   | 14     | 2           | 4      | NZ        |
| A, $0000   | 05     | 3           | 4      | NZ        |
| A, $0000+X | 15     | 3           | 5      | NZ        |
| A, $0000+Y | 16     | 3           | 5      | NZ        |
| A, ($00+X) | 07     | 2           | 6      | NZ        |
| A, ($00)+Y | 17     | 2           | 6      | NZ        |
| (X), (Y)   | 19     | 1           | 5      | NZ        |
| $00, $00   | 09     | 3           | 6      | NZ        |
| $00, #$00  | 18     | 3           | 5      | NZ        |

## `EOR`

`EOR` instructions perform logical bitwise exclusive OR (XOR or EOR) on the operands, then store the result in the target operand.

| Operands   | Opcode | Total bytes | Cycles | Flags set |
| ---------- | ------ | ----------- | ------ | --------- |
| A, #$00    | 48     | 2           | 2      | NZ        |
| A, (X)     | 46     | 1           | 3      | NZ        |
| A, $00     | 44     | 2           | 3      | NZ        |
| A, $00+X   | 54     | 2           | 4      | NZ        |
| A, $0000   | 45     | 3           | 4      | NZ        |
| A, $0000+X | 55     | 3           | 5      | NZ        |
| A, $0000+Y | 56     | 3           | 5      | NZ        |
| A, ($00+X) | 47     | 2           | 6      | NZ        |
| A, ($00)+Y | 57     | 2           | 6      | NZ        |
| (X), (Y)   | 59     | 1           | 5      | NZ        |
| $00, $00   | 49     | 3           | 6      | NZ        |
| $00, #$00  | 58     | 3           | 5      | NZ        |

## `INC`

`INC` instructions increment the data in the operand.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | BC     | 1           | 2      | NZ        |
| X       | 3D     | 1           | 2      | NZ        |
| Y       | FC     | 1           | 2      | NZ        |
| $00     | AB     | 2           | 4      | NZ        |
| $00+X   | BB     | 2           | 5      | NZ        |
| $0000   | AC     | 3           | 5      | NZ        |

## `DEC`

`INC` instructions decrement the data in the operand.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 9C     | 1           | 2      | NZ        |
| X       | 1D     | 1           | 2      | NZ        |
| Y       | DC     | 1           | 2      | NZ        |
| $00     | 8B     | 2           | 4      | NZ        |
| $00+X   | 9B     | 2           | 5      | NZ        |
| $0000   | 8C     | 3           | 5      | NZ        |

## `ASL`

`ASL` instructions perform (one) in-place arithmetic shift left on the operand; shifted-out one bits are stored in the carry flag and zero bits are shifted in.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 1C     | 1           | 2      | NZC       |
| $00     | 0B     | 2           | 4      | NZC       |
| $00+X   | 1B     | 2           | 5      | NZC       |
| $0000   | CC     | 3           | 5      | NZC       |

## `LSR`

`LSR` instructions perform (one) in-place shift right on the operand; shifted-out one bits are stored in the carry flag and zero bits are shifted into the top.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 5C     | 1           | 2      | NZC       |
| $00     | 4B     | 2           | 4      | NZC       |
| $00+X   | 5B     | 2           | 5      | NZC       |
| $0000   | 4C     | 3           | 5      | NZC       |

## `ROL`

`ROL` instructions perform (one) in-place left rotation on the operand; the shifted-around bit is stored in the carry flag.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 3C     | 1           | 2      | NZC       |
| $00     | 2B     | 2           | 4      | NZC       |
| $00+X   | 3B     | 2           | 5      | NZC       |
| $0000   | 2C     | 3           | 5      | NZC       |

## `ROR`

`ROR` instructions perform (one) in-place right rotation on the operand; the shifted-around bit is stored in the carry flag.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 7C     | 1           | 2      | NZC       |
| $00     | 6B     | 2           | 4      | NZC       |
| $00+X   | 7B     | 2           | 5      | NZC       |
| $0000   | 6C     | 3           | 5      | NZC       |

## `XCN`

The `XCN` instruction exchanges the nybbles of A in-place, meaning that the bits 0-3 are moved to the bits 7-4 and vice-versa.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | 9F     | 1           | 5      | NZ        |

## `MOVW`

`MOVW` instructions perform a 16-bit move ("wide" move) between the combined YA register and two memory addresses in the direct page.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| YA, $00  | BA     | 2           | 5      | NZ        |
| $00, YA  | DA     | 2           | 4      |           |

## `INCW`

The `INCW` instruction performs an increment that carries between the two bytes.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| $00     | 3A     | 2           | 6      | NZ        |

## `DECW`

The `DECW` instruction performs a decrement that carries between the two bytes.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| $00     | 1A     | 2           | 6      | NZ        |

## `ADDW`

The `ADDW` instruction performs a 16-bit addition without carry.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| YA, $00  | 7A     | 2           | 5      | NVHZC     |

## `SUBW`

The `SUBW` instruction performs a 16-bit subtraction without carry.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| YA, $00  | 9A     | 2           | 5      | NVHZC     |

## `CMPW`

The `CMPW` instruction performs a 16-bit subtraction without carry, but the result is not stored back into YA similar to the 8-bit `CMP`.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| YA, $00  | 5A     | 2           | 4      | NZ        |

## `MUL`

The `MUL` instruction multiplies the Y register with the A register and stores the 16-bit result of this 8-bit multiplication back into the YA register. This multiplication is unsigned.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| YA      | CF     | 1           | 9      | NZ        |

## `DIV`

The `DIV` instruction divides the YA register by the X register with unsigned integer division. The quotient is stored in A, and the remainder is stored in Y.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| YA      | 9E     | 1           | 12     | NVHZ      |

## `DAA`

The `DAA` instruction provides binary coded decimal (BCD) support together with the `DAS` instruction. If the half-carry flag H is set, decimal adjust for addition will add 6 to convert the result back to decimal. Note that the H flag is the only required flag to be set correctly for this instruction to work, as opposed to e.g. the Z80 which also requires the normal carry flag.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | DF     | 1           | 3      | NZC       |

## `DAS`

The `DAS` instruction provides binary coded decimal (BCD) support together with the `DAA` instruction. If the half-carry flag H is set, decimal adjust for subtraction will subtract 6 to convert the result back to decimal. Note that the H flag is the only required flag to be set correctly for this instruction to work, as opposed to e.g. the Z80 which also requires the normal carry flag.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| A       | BE     | 1           | 3      | NZC       |

## `BRA`, `BEQ`, `BNE`, `BCS`, `BCC`, `BVS`, `BVC`, `BMI`, `BPL`

The main branching instructions use relative addressing to jump to any address within a -128, +127 distance from the following instruction. The cycle count varies depending on whether the branch is taken or not. No branching instruction sets any flags.

| Mnemonic | Condition                  | Operand | Opcode | Total bytes | Cycles (branch taken) | Cycles (branch not taken) |
| -------- | -------------------------- | ------- | ------ | ----------- | --------------------- | ------------------------- |
| BRA      | -                          | rel     | 2F     | 2           | 4                     | -                         |
| BEQ      | Z flag set (equal)         | rel     | F0     | 2           | 4                     | 2                         |
| BNE      | Z flag not set (not equal) | rel     | D0     | 2           | 4                     | 2                         |
| BCS      | C flag set                 | rel     | B0     | 2           | 4                     | 2                         |
| BCC      | C flag not set             | rel     | 90     | 2           | 4                     | 2                         |
| BVS      | V flag set                 | rel     | 70     | 2           | 4                     | 2                         |
| BVC      | V flag not set             | rel     | 50     | 2           | 4                     | 2                         |
| BMI      | N flag set                 | rel     | 30     | 2           | 4                     | 2                         |
| BPL      | N flag not set             | rel     | 10     | 2           | 4                     | 2                         |

## `BBS`

The branch if bit set instruction branches if the specified bit of the memory at the direct page address is set. Note that the opcode is combined with the bit index; see [Bit indexing](../reference#bit-indexing) for details on how this works.

| Operands     | Opcode | Total bytes | Cycles (branch taken) | Cycles (branch not taken) |
| ------------ | ------ | ----------- | --------------------- | ------------------------- |
| $00.bit, rel | 03     | 3           | 7                     | 5                         |

## `BBC`

The branch if bit set instruction branches if the specified bit of the memory at the direct page address is cleared. Note that the opcode is combined with the bit index; see [Bit indexing](../reference#bit-indexing) for details on how this works.

| Operands     | Opcode | Total bytes | Cycles (branch taken) | Cycles (branch not taken) |
| ------------ | ------ | ----------- | --------------------- | ------------------------- |
| $00.bit, rel | 13     | 3           | 7                     | 5                         |

## `CBNE`

The `CBNE` instruction compares the A register with memory in the direct page, like `CMP` can also do. Then, it branches if the two are not equal. As opposed to `CMP` and `BNE`, this instruction does not set or inspect any flags.

| Operands   | Opcode | Total bytes | Cycles (branch taken) | Cycles (branch not taken) |
| ---------- | ------ | ----------- | --------------------- | ------------------------- |
| $00, rel   | 2E     | 3           | 7                     | 5                         |
| $00+X, rel | DE     | 3           | 8                     | 6                         |

## `DBNZ`

The `DBNZ` instruction decrements the operand and takes the branch if the result was not zero. This is intended for fast and compact loops, for this the memory location or the Y register can store the loop counter and the instruction branches to the top of the loop.

| Operands | Opcode | Total bytes | Cycles (branch taken) | Cycles (branch not taken) |
| -------- | ------ | ----------- | --------------------- | ------------------------- |
| $00, rel | 6E     | 3           | 7                     | 5                         |
| Y, rel   | FE     | 2           | 6                     | 4                         |

## `JMP`

The `JMP` instruction performs an unconditional absolute jump to any address. The x-indexed mode will read the jump target from the offset memory location and the memory location following that.

| Operand   | Opcode | Total bytes | Cycles |
| --------- | ------ | ----------- | ------ |
| $0000     | 5F     | 3           | 3      |
| ($0000+X) | 1F     | 3           | 6      |

## `CALL`

The `CALL` instruction jumps like the `JMP` instruction, but also stores the program counter to the stack.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| $0000   | 3F     | 3           | 8      |

## `PCALL`

The `PCALL` instruction obtains an 8-bit operand, but this operand is added to FF00 to obtain the memory address to call to. Therefore, `PCALL` calls into the so-called "special page" at the top of the address space.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| $00     | 4F     | 2           | 6      |

## `TCALL`

The `TCALL` instruction encodes the operand within the opcode (see [`TCALL` encoding](../reference#tcall-encoding)), and this operand serves as a table index. The word in memory at `FFC0+2(15-n)` is used as the memory address to call to. In other words: `TCALL` calls to a pointer found in the subroutine pointer table at FFC0, where the table starts at the highest word and proceeds downwards.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| n       | 01     | 1           | 8      |

## `BRK`

The `BRK` instruction causes a software interrupt. Like with a call, the program counter is pushed to the stack, followed by the flag register. Then, execution jumps to FFDE where the software interrupt handler must be located.

| Opcode | Total bytes | Cycles | Flags set |
| ------ | ----------- | ------ | --------- |
| 0F     | 1           | 8      | B=0, I=1  |

## `RET`

The `RET` instruction returns from a subroutine called with a `CALL`-like instruction by loading the program counter from the stack.

| Opcode | Total bytes | Cycles |
| ------ | ----------- | ------ |
| 6F     | 1           | 5      |

## `RET1`

The `RET1` instruction returns from a software interrupt. The program counter and flag register are restored from the stack where they were pushed by `BRK`.

| Opcode | Total bytes | Cycles | Flags set           |
| ------ | ----------- | ------ | ------------------- |
| 7F     | 1           | 6      | Restored from stack |

## `PUSH`

The `PUSH` instruction allows to manually move registers to the stack, including the flag register. Note that `CALL` behavior cannot fully be emulated by this instruction, as it cannot push the program counter to the stack.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| A       | 2D     | 1           | 4      |
| X       | 4D     | 1           | 4      |
| Y       | 6D     | 1           | 4      |
| PSW     | 0D     | 1           | 4      |

## `POP`

The `POP` instruction allows to manually restore registers from the stack, including the flag register.

| Operand | Opcode | Total bytes | Cycles | Flags set           |
| ------- | ------ | ----------- | ------ | ------------------- |
| A       | AE     | 1           | 4      |                     |
| X       | CE     | 1           | 4      |                     |
| Y       | EE     | 1           | 4      |                     |
| PSW     | 8E     | 1           | 4      | Restored from stack |

## `SET1`

The `SET1` instruction sets a single bit in the direct page. Note that the opcode is combined with the bit index; see [Bit indexing](../reference#bit-indexing) for details on how this works.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| $00.bit | 02     | 2           | 4      |

## `CLR1`

The `CLR1` instruction clears a single bit in the direct page. Note that the opcode is combined with the bit index; see [Bit indexing](../reference#bit-indexing) for details on how this works.

| Operand | Opcode | Total bytes | Cycles |
| ------- | ------ | ----------- | ------ |
| $00.bit | 12     | 2           | 4      |

## `TSET1`

The `TSET1` instruction performs logical AND between the A register and the data in memory in order to set processor flags, but the A register is not modified. Then, all bits specified by the A register are set at this same memory address.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| $0000   | 0E     | 3           | 6      | NZ        |

## `TCLR1`

The `TCLR1` instruction performs logical AND between the A register and the data in memory in order to set processor flags, but the A register is not modified. Then, all bits specified by the A register are cleared at this same memory address.

| Operand | Opcode | Total bytes | Cycles | Flags set |
| ------- | ------ | ----------- | ------ | --------- |
| $0000   | 4E     | 3           | 6      | NZ        |

## `AND1`, `OR1`, `EOR1`

The `AND1`, `OR1`, `EOR1` instruction perform their respective logical operation between the carry flag and the specified bit of the value in memory; this bit might be negated. The result is stored back into the carry flag.

| Mnemonic | Operands      | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------------- | ------ | ----------- | ------ | --------- |
| AND1     | C, $0000.bit  | 4A     | 3           | 4      | C         |
| AND1     | C, /$0000.bit | 6A     | 3           | 4      | C         |
| OR1      | C, $0000.bit  | 0A     | 3           | 5      | C         |
| OR1      | C, /$0000.bit | 2A     | 3           | 5      | C         |
| EOR1     | C, $0000.bit  | 8A     | 3           | 5      | C         |

## `NOT1`

The `NOT1` instruction negates one bit of a value in memory.

| Operand   | Opcode | Total bytes | Cycles |
| --------- | ------ | ----------- | ------ |
| $0000.bit | EA     | 3           | 5      |

## `MOV1`

The `MOV1` instruction moves one bit of a value in memory into the carry flag or vice-versa.

| Operands     | Opcode | Total bytes | Cycles | Flags set |
| ------------ | ------ | ----------- | ------ | --------- |
| C, $0000.bit | AA     | 3           | 4      | C         |
| $0000.bit, C | CA     | 3           | 6      |           |

## `CLRC`, `SETC`, `NOTC`, `CLRV`, `CLRP`, `SETP`, `EI`, `DI`

The flag manipulation instructions change one flag's state each. Note that the `EI` and `DI` instructions do not have much effect on the S-SMP, as it has no interrupt sources.

| Mnemonic | Opcode | Total bytes | Cycles | Flags set          |
| -------- | ------ | ----------- | ------ | ------------------ |
| CLRC     | 60     | 1           | 2      | C=0                |
| SETC     | 80     | 1           | 2      | C=1                |
| NOTC     | ED     | 1           | 3      | C=!C (logical not) |
| CLRV     | E0     | 1           | 2      | V=0, H=0           |
| CLRP     | 20     | 1           | 2      | P=0                |
| SETP     | 40     | 1           | 2      | P=1, I=0           |
| EI       | A0     | 1           | 3      | I=1                |
| DI       | C0     | 1           | 3      | I=0                |

## `NOP`

The `NOP` instruction performs no operation.

| Opcode | Total bytes | Cycles |
| ------ | ----------- | ------ |
| 00     | 1           | 2      |

## `SLEEP`

The `SLEEP` instruction puts the CPU into a low-power sleep state until an interrupt is received. Because the S-SMP has no external interrupt sources, this has the same result as the `STOP` instruction.

| Opcode | Total bytes | Cycles |
| ------ | ----------- | ------ |
| EF     | 1           | 3      |

## `STOP`

The `STOP` instruction halts the CPU in a low-power mode. This state can only be exited via a reset.

| Opcode | Total bytes | Cycles |
| ------ | ----------- | ------ |
| FF     | 1           | 3      |
