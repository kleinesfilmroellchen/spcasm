# Instructions

All SPC700 instructions have the form `MNEMONIC TARGET,SOURCE`. The addressable memory range and flags set depend on the exact opcode.

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
| (X), A     | C6     | 1           | 4      | -         |
| (X)+, A    | AF     | 1           | 4      | -         |
| $00, A     | C4     | 2           | 4      | -         |
| $00+X, A   | D4     | 2           | 5      | -         |
| $0000, A   | C5     | 3           | 5      | -         |
| $0000+X, A | D5     | 3           | 6      | -         |
| $0000+Y, A | D6     | 3           | 6      | -         |
| ($00+X), A | C7     | 2           | 7      | -         |
| ($00)+Y, A | D7     | 2           | 7      | -         |
| $00, X     | D8     | 2           | 4      | -         |
| $00+Y, X   | D9     | 2           | 5      | -         |
| $0000, X   | C9     | 3           | 5      | -         |
| $00, Y     | CB     | 2           | 4      | -         |
| $00+X, Y   | DB     | 2           | 5      | -         |
| $0000, Y   | CC     | 3           | 5      | -         |
| A, X       | 7D     | 1           | 2      | NZ        |
| A, Y       | DD     | 1           | 2      | NZ        |
| X, A       | 5D     | 1           | 2      | NZ        |
| Y, A       | FD     | 1           | 2      | NZ        |
| X, SP      | 9D     | 1           | 2      | NZ        |
| SP, X      | BD     | 1           | 2      | -         |
| $00, $00   | FA     | 3           | 5      | -         |
| $00, #$00  | 8F     | 3           | 5      | -         |

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

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | BC     | 1           | 2      | NZ        |
| X        | 3D     | 1           | 2      | NZ        |
| Y        | FC     | 1           | 2      | NZ        |
| $00      | AB     | 2           | 4      | NZ        |
| $00+X    | BB     | 2           | 5      | NZ        |
| $0000    | AC     | 3           | 5      | NZ        |

## `DEC`

`INC` instructions decrement the data in the operand.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 9C     | 1           | 2      | NZ        |
| X        | 1D     | 1           | 2      | NZ        |
| Y        | DC     | 1           | 2      | NZ        |
| $00      | 8B     | 2           | 4      | NZ        |
| $00+X    | 9B     | 2           | 5      | NZ        |
| $0000    | 8C     | 3           | 5      | NZ        |

## `ASL`

`ASL` instructions perform (one) in-place arithmetic shift left on the operand; shifted-out one bits are stored in the carry flag and zero bits are shifted in.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 1C     | 1           | 2      | NZC       |
| $00      | 0B     | 2           | 4      | NZC       |
| $00+X    | 1B     | 2           | 5      | NZC       |
| $0000    | CC     | 3           | 5      | NZC       |

## `LSR`

`LSR` instructions perform (one) in-place shift right on the operand; shifted-out one bits are stored in the carry flag and zero bits are shifted into the top.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 5C     | 1           | 2      | NZC       |
| $00      | 4B     | 2           | 4      | NZC       |
| $00+X    | 5B     | 2           | 5      | NZC       |
| $0000    | 4C     | 3           | 5      | NZC       |

## `ROL`

`ROL` instructions perform (one) in-place left rotation on the operand; the shifted-around bit is stored in the carry flag.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 3C     | 1           | 2      | NZC       |
| $00      | 2B     | 2           | 4      | NZC       |
| $00+X    | 3B     | 2           | 5      | NZC       |
| $0000    | 2C     | 3           | 5      | NZC       |

## `ROL`

`ROR` instructions perform (one) in-place right rotation on the operand; the shifted-around bit is stored in the carry flag.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 7C     | 1           | 2      | NZC       |
| $00      | 6B     | 2           | 4      | NZC       |
| $00+X    | 7B     | 2           | 5      | NZC       |
| $0000    | 6C     | 3           | 5      | NZC       |

## `XCN`

The `XCN` instruction exchanges the nybbles of A in-place, meaning that the bits 0-3 are moved to the bits 7-4 and vice-versa.

| Operands | Opcode | Total bytes | Cycles | Flags set |
| -------- | ------ | ----------- | ------ | --------- |
| A        | 9F     | 1           | 5      | NZ        |
