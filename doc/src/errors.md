# Error reference

spcasm reports various kinds of errors, warnings and advice. Errors always abort the assembly process. Warnings inform you of things that might not have the behavior you want, and point out what the assembler actually did. Advice, often called "Infos" in IDEs, informs you of assembler behavior that you are probably expecting, but which is nice to know about anyways.

All of these three are often collectively called errors, as they can be turned into hard errors with the right settings (see [Usage](usage.md)). Each error can be uniquely identified by its error code, which starts with `spcasm::` by convention and follows a Rust namespace-like syntax. The various categories group errors related to common spcasm features. (There are no numeric error codes, not even behind the scenes; spcasm always exits with exit code 1 if an error occurs.)

In the following, each error is listed with an example and a detailed explanation of what the problem is and how to deal with it. Note that the exact reports in the examples are not always updated and might not reflect what spcasm currently reports, but the error code is usually stable.

- auto-gen TOC;
  {:toc}

## Advice

### spcasm::reference::non_direct_page

```notrycmd
  ☞ This reference "data" has an 8-bit value, did you want to use it in direct page addressing?
    ╭─[tests/opcodes.s:10:1]
 10 │    MOV A,$3320+X ;= F5 20 33
 11 │    MOV A,$FF00+Y ;= F6 00 FF
 12 │    MOV A,($40+X) ;= E7 40
 13 │ data:         ; @ 13
    · ──┬─
    ·   ╰── Might point at a direct page address
 14 │    MOV A,(%00100000)+Y ;= F7 20
    ·    ──────────────┬─────────────
    ·                  ╰── Memory address 13
 15 │    MOV X,#$60    ;= CD 60
 16 │    MOV X,$9A     ;= F8 9A
 17 │    MOV X,$2E+Y   ;= F9 2E
    ╰────
  help: Use a forced direct page addressing mnemonic by suffixing `.b`
```

Many SPC700 instructions can save space and clock cycles by using direct page addressing when accessing either the zeroth or first page of memory (see [the reference](reference/)). Spcasm is able to sometimes deduce when it is possible to use such an instruction, but not always. Therefore, spcasm warns you that it found out this address is within a direct page, but it couldn't actually assemble the instruction to use direct-page addressing. The mnemonic suffix `.b` is available to force any instruction to use direct page addressing if possible.

Note: No known assembly code causes this message anymore, since spcasm gained the ability to deduce direct page addressing in most circumstances.

### spcasm::arch::valid

```trycmd
$ spcasm -w all -W arch::valid tests/opcodes.s
? 1
spcasm::arch::valid

  ☞ Legal architecture directive ignored
   ╭─[tests/opcodes.s:1:1]
 1 │ arch spc700-raw
   · ───────┬───────
   ·        ╰── `arch` directive
 2 │ org 0
 3 │ startpos
 4 │ start:        ; @ 0
   ╰────
  help: spcasm supports `arch` directives for compatibility with the Asar
        multi-architecture assembler. This arch directive points to the spc700
        architecture and is therefore safely ignored.


```

spcasm has a whole collection of features targeted at compatibility with the popular [Asar](https://rpghacker.github.io/asar/manual/) multi-architecture SNES patch assembler. As a multi-architecture assembler, Asar allows you to [specify the architecture of an assembly file](https://rpghacker.github.io/asar/manual/#archs) and even switch between them. In order to allow compilation with pure SPC-700 assembly originally written for Asar, spcasm therefore recognizes the `arch` directive and accepts any SPC-700-related architecture specifications like `spc700`. This advice exists so that you are informed of ignored architecture directives and to allow you to forbid the use of such directives.

## Warnings

### spcasm::directive::negative_repeatcount

```trycmd
$ spcasm -w all -W spcasm::directive::negative_repeatcount tests/repeat.spcasmtest
? 1
spcasm::directive::negative_repeatcount

  ⚠ Repetition count is negative and will be clamped to zero
    ╭─[tests/repeat.spcasmtest:22:1]
 19 │     bne label_before_repeat ;= D0 FE
 20 │     
 21 │     ; negative repeats act like 0 but produce a warning
 22 │ ╭─▶ repeat (-2)
    · │           ─┬
    · │            ╰── Repeat count resolves to -2
 23 │ │       nop ;= FF
 24 │ ├─▶ endrepeat
    · ╰──── While expanding this repeat directive
 25 │     
 26 │     increasing_number_table:
 27 │     repeat 100
    ╰────
  help: Use repeat count zero to not repeat this block at all


```

The [`repeat` directive](reference/directives.md#repeat) accepts all repeat count values, even negative ones. However, since a negative repeat doesn’t make practical sense, it simply doesn’t repeat the block at all and acts like repeat count zero. Usually, this is unintentional, and the repeat count should be replaced with 0.

### spcasm::value_too_large

```trycmd
$ spcasm -W value_too_large -w relative_offset_too_large tests/parse.spcasmtest
? 1
spcasm::value_too_large

  ⚠ The value `FFFFFFFFFFFFFF4D` is being used as a 8-bit operand here, but it
  │ is larger than this. The extra upper bits are truncated.
    ╭─[tests/parse.spcasmtest:30:10]
 27 │ mov a, # 8 >>2 ;= E8 02
 28 │ mov a , #$ff &%11 ;= E8 03
 29 │  mov a,# $0F |$f0 ;= E8 FF
 30 │ mov a ,#~%10110010 ;= E8 4D
    ·          ────┬────
    ·              ╰── 8-bit operand
 31 │ mov a, #$9f ^ $78 ^$9F ;= E8 78
 32 │ mov a,#6**3 ;= E8 D8
 33 │ mov a,#(3 == 5) ;= E8 00
    ╰────
  help: If this was intentional, explicitly truncate the value.

spcasm::value_too_large

  ⚠ The value `ABCD` is being used as a 8-bit operand here, but it is larger
  │ than this. The extra upper bits are truncated.
    ╭─[tests/parse.spcasmtest:45:9]
 42 │ mov a,#(7 <=12) ;= E8 01
 43 │ 
 44 │ + mov a,$abcd    ;= E5 CD AB
 45 │ mov.b a,$abcd  ;= E4 CD
    ·         ──┬──
    ·           ╰── 8-bit operand
 46 │ mov.w a,$00cd  ;= E5 CD 00
 47 │ 
 48 │ db 'x', '/x67', '//', '/"', '/''
    ╰────
  help: If this was intentional, explicitly truncate the value.


```

In most places, numbers have a limited size, often 8 or 16 bits. However, user-specified numbers are initially limited much higher, to 64-bit signed numbers, in order to allow computations that involve large intermediate values. When using those large numbers in these bit-limited places, the higher bits are simply cut off or truncated. This can be surprising depending on what exact instruction or directive the number is used in, so spcasm issues a specific warning by default.

### spcasm::relative_offset_too_large

```trycmd
$ spcasm -W relative_offset_too_large tests/relative-large-offset.spcasmtest
? 1
spcasm::relative_offset_too_large

  ⚠ The relative offset to address `0200` is out of range, the result will be
  │ wrong.
   ╭─[tests/relative-large-offset.spcasmtest:3:1]
 1 │ org $200
 2 │ 
 3 │ start:
   · ──┬──
   ·   ╰── Target address `0200`
 4 │     nop
 5 │ 
 6 │ fillbyte 0
 7 │ fill $100
 8 │ 
 9 │     bra start
   ·         ──┬──
   ·           ╰── Difference of -259 to current address
   ╰────
  help: The current address is `0302` and therefore the difference is -259.
        This difference exceeds the range [-128, 127] and will wrap around,
        probably leading to incorrect code. If you are using a relative jump,
        consider using a trampoline to cover larger jump distances.


```

Relative offsets encoded as signed two's-complement in an 8-bit number have a range from -128 to +127, both inclusive. These kinds of offsets are primarily used in branching instructions, which do not encode an absolute address to jump to (unlike jump instructions), but encode an offset from the next instruction address, since branches rarely need to jump more than a couple dozen instructions forward or backward. If the distance to the target address is too large, however, the value will overflow 8 bits in various ways and lead to a highly incorrect relative offset. This warning is especially important since it does not only inform you about incorrect data (like `spcasm::value_too_large`), but about incorrect / misassembled code which will not behave like you expect it to.

Concerning branch instructions in particular, depending on your code situation there are several solutions to this problem:

- If you can move code around, such as when the target of the branch is actually in another subroutine, you can move the branch target and the branch instruction closer together.
- If some code between the instruction and its target can be extracted to a subroutine, you can move that subroutine far behind or before this code and therefore decrease the distance.
- If the code positions are rather rigid, but you can insert a couple of instructions less than +-127 bytes from the branch source, you can insert a trampoline. This is a static jump instruction that jumps to the actual target, and the branch instruction jumps to this trampoline. Therefore, the conditional jump is executed in two steps, and you can even group multiple trampoline jump statements together. The surrounding code of the trampoline must be careful not to actually execute it; this is less of a problem when the trampoline is inserted in between two subroutines.

### spcasm::syntax::number_identifier

```trycmd
$ spcasm -w all -W syntax::number_identifier tests/number-identifier.spcasmtest
? 1
spcasm::syntax::number_identifier

  ⚠ "07x" was expected to be a number, but it is parsed as an identifier
  │ instead
   ╭─[tests/number-identifier.spcasmtest:8:2]
 5 │ 
 6 │ 
 7 │ 
 8 │ .07x nop
   ·  ─┬─
   ·   ╰── Parse error: invalid digit found in string
   ╰────
  help: Identifiers starting with numbers is an Asar compatibility feature,
        but it is not recommended since it can lead to weird errors later on.


```

In Asar, identifiers can start with numbers, and in fact can consist entirely of numbers when their use is unambiguous. (The rules on this are not precisely documented, in fact.) In particular, a popular use of numeric identifiers are numbered local labels, such as `.01`, `.02` etc., where due to the leading `.` there never is any ambiguity. For compatibility reasons, spcasm supports this syntax, but it will issue a warning when an identifier starts with numbers. This is because if the identifier entirely consists of numbers, whether or not it is recognized as a reference entirely depends on the circumstances, and spcasm will usually not issue an error if the number appears in a valid context. That can mean that either the assembly output is wrong, or the error is very strange. Since warnings can be turned into hard errors, this "feature" can therefore be disabled, and doing so is strongly recommended.

## Errors

### spcasm::arch::invalid

```trycmd
$ spcasm -w all tests/errors/unsupported-arch.spcasmtest
? 1
spcasm::arch::invalid

  × Unknown architecture `x65c816` specified
   ╭─[tests/errors/unsupported-arch.spcasmtest:1:1]
 1 │ arch x65c816
   · ──────┬─────
   ·       ╰── `arch` directive
 2 │ org 0
   ╰────
  help: spcasm supports `arch` directives for compatibility with the Asar
        multi-architecture assembler. This directive specifies that the
        architecture of the assembly source is not (completely) SPC700,
        therefore spcasm cannot assemble this file.


```

See [arch::valid](#spcasmarchvalid); when compiling files originally targeted at Asar this error detects early when you are trying to compile non-SPC700 assembly.

### spcasm::directive

This category contains directive-related errors.

#### spcasm::directive::duplicate_startpos

```trycmd
$ spcasm -w all tests/errors/duplicate-startpos.spcasmtest
? 1
spcasm::directive::duplicate_startpos

  × Duplicate startpos directive
   ╭─[tests/errors/duplicate-startpos.spcasmtest:6:1]
 3 │ nop
 4 │ 
 5 │ org $300
 6 │ startpos
   · ────┬───
   ·     ╰── `startpos` directive
 7 │ nop
   ╰────
  help: the `startpos` directive defines the execution entry point of the ROM
        after it was loaded. There can only be one entry point.


```

The [`startpos` directive](reference/directives.md#startpos) can only be specified once, since there can only be one program entry point.

#### spcasm::directive::invalid_directive_option

```trycmd
$ spcasm -w all tests/errors/invalid-brr-option.spcasmtest
? 1
spcasm::directive::invalid_directive_option

  × Invalid option `wrongoption` for directive `brr`
   ╭─[tests/errors/invalid-brr-option.spcasmtest:1:1]
 1 │ brr "../song.wav" wrongoption
   · ────────┬──────── ─────┬─────
   ·         │              ╰── Invalid option specified here
   ·         ╰── `brr` directive defined here
   ╰────
  help: The valid options are `nodirectory`, `autotrim`.


```

Some directives accept options as identifiers, but the one you used was not one of them. Refer to the [directive documentation](reference/directives.md) to learn more about supported options for these directives.

#### spcasm::directive::invalid_range

```trycmd
$ spcasm -w all tests/errors/parser-invalid-range.spcasmtest
? 1
spcasm::directive::invalid_range

  × 4 is greater than 2
   ╭─[tests/errors/parser-invalid-range.spcasmtest:2:23]
 1 │ org 0
 2 │ incbin "../binary.bin":4-2
   ·                       ─┬
   ·                        ╰── In this range
   ╰────
  help: Switch the range limits around: `2-4`.


```

For range specifications, like when including binary files, the Asar style range syntax is a `start-end` format. Obviously, the start then needs to be before (or the same as) the end. Often you just accidentally swapped these limits.

#### spcasm::directive::math_pri_unsupported

```trycmd
$ spcasm -w all tests/errors/math-pri.spcasmtest
? 1
spcasm::directive::math_pri_unsupported

  × `math pri` is not supported by spcasm
   ╭─[tests/errors/math-pri.spcasmtest:2:1]
 1 │ 
 2 │ math pri off
   · ──┬─
   ·   ╰── `math pri` directive
   ╰────
  help: This directive switches between normal operator precedence and "dumb"
        left-to-right precedence in the Asar multi-architecture assembler.
        This feature is intended for xkas compatibility, but spcasm does not
        consider this an important feature. Therefore, spcasm cannot switch
        math priority mode. Please check your math expressions manually if you
        are using `math pri off`.


```

`math pri` is a directive that Asar supports for xkas compatibility. By default, Asar operates in left-to-right math priority like xkas, and proper priority can be enabled with `math pri on`. spcasm does not have this feature, however, to allow easier switching over to spcasm, the directive is detected and an error is reported. There are two courses of action:

- If your assembly entirely or mostly uses `math pri on`, you can just remove the directive, as spcasm uses proper math priority like Asar with `math pri on`.
- If your assembly entirely or mostly uses `math pri off`, you can bracket your math expressions manually to emulate the left-to-right precedence behavior of xkas. Then, remove the directive.

#### spcasm::directive::missing_fill_pad_parameter

```trycmd
$ spcasm -w all tests/errors/missing-fill-value.spcasmtest
? 1
spcasm::directive::missing_fill_pad_parameter

  × No value specified for `fill`
   ╭─[tests/errors/missing-fill-value.spcasmtest:2:1]
 1 │ org 0
 2 │ fill 7
   · ───┬──
   ·    ╰── `fill` defined here
   ╰────
  help: `fill` needs a value, which is specified separately via another
        directive like `fillbyte`. Such a directive was not found.


```

The `fill`, `fill align`, and `pad` directives have the value to be used for filling specified in a separate directive. For `fill` and `fill align`, these are `fillbyte`, `fillword`, `filllong`, `filldword`, and for `pad` they are `padbyte`, `padword`, `padlong`, `paddword`. Note that segments have no effect on these directives, so you must precede the fill directive with the value specification directive in source code. (Yes, these directives have a weird design for Asar compatibility reasons.)

#### spcasm::directive::range_out_of_bounds

```trycmd
$ spcasm -w all tests/errors/range-out-of-bounds.spcasmtest
? 1
spcasm::directive::range_out_of_bounds

  × The range 20000-50000 is out of bounds for the input file "../binary.bin"
   ╭─[tests/errors/range-out-of-bounds.spcasmtest:2:1]
 1 │ org 0
 2 │ incbin "../binary.bin":20000-50000
   · ─────────────────┬────────────────
   ·                  ╰── Out of bounds range defined here
   ╰────
  help: The input's length is 6.


```

When providing a range for a binary file that is included, the range is allowed to exceed the end of the binary, at which point just everything until the binary's end is included. However, the start point must lie within the binary.

#### spcasm::directive::recursion_limit

```trycmd
$ spcasm -w all tests/errors/recursive-macro.spcasmtest
? 1
spcasm::directive::recursion_limit

  × Maximum recursion depth 1000 was exceeded while expanding 'recursive'
   ╭─[tests/errors/recursive-macro.spcasmtest:5:3]
 2 │ 
 3 │ macro recursive
 4 │   mov a,#4
 5 │   %recursive()
   ·   ──────┬─────
   ·         ╰── While trying to expand this
 6 │ endmacro
 7 │ 
 8 │ recursion_start:
   ╰────
  help: This is most likely caused by an infinite recursion in a macro calling
        itself. On the command line, use `--macro-recursion-limit` to increase
        the limit.


```

Directives and user macros may be used within other directives and macros, but they must at some point fully resolve to real data. This may not happen if a macro is self-recursive infinitely. Because the recursion is hard to detect in an abstract manner, spcasm employs a recursion depth limit that will stop expanding directives and macros after some point. This limit is very large by default and should suffice for all reasonable situations. However, if you _really_ need to recurse more, and your recursion is in fact finite, you can change this limit on the command line with `--macro-recursion-limit`.

#### spcasm::directive::references_as_argument

```trycmd
$ spcasm -w all tests/errors/label-in-org.spcasmtest
? 1
spcasm::directive::references_as_argument

  × Invalid use of labels in an argument for `org`
   ╭─[tests/errors/label-in-org.spcasmtest:6:1]
 3 │ dp_label:
 4 │    nop       ; dummy
 5 │ 
 6 │ org dp_label
   · ─┬─ ────┬───
   ·  │      ╰── This directive argument
   ·  ╰── This directive
   ╰────
  help: Because the directive argument can determine a reference's position,
        resolving the argument value is not generally possible. For this
        reason, references are not allowed to be used in a directive argument.


```

Some directives, notably the `org` directive, disallow the use of references and other dynamic expressions in their argument(s) as that easily creates unresolvable circular dependencies. For example, consider this assembly code:

```asm
org loop
	nop
loop:
	nop
```

Let's ignore the org directive and assume that loop is at memory location 1. Then, however, loop must be at location 2 as the org directive now reads `org 1` (the nop instruction takes 1 byte). Then, however, loop must be at location 3 as the org directive now reads `org 3`. This line of reasoning will recurse to infinity and the above code is ill-formed.

spcasm doesn't yet have the capability of distinguishing between such a case and one where there is no problem at all, like:

```asm
custom_code_bank = $4000

org custom_code_bank
	nop
	; custom code...
```

Therefore, for now, spcasm disallows the use of references in these cases entirely.

#### spcasm::directive::sample_table_too_large

```trycmd
$ spcasm -w all tests/errors/sample-table-too-large.spcasmtest
? 1
spcasm::directive::sample_table_too_large

  × BRR sample table has more than 256 entries
   ╭─[tests/errors/sample-table-too-large.spcasmtest:3:1]
 1 │ org 0
 2 │ 
 3 │ sampletable
   · ─────┬─────
   ·      ╰── automatic sample table generated here with 384 entries
 4 │ 
 5 │ macro yoshi
 6 │     brr "../yoshi.wav": 0-8
   ╰────
  help: BRR sample tables cannot contain more than 256 entries. The
        autogenerated table contains all BRR samples without the `nodirectory`
        option, which includes 384 samples in this case. To reduce the number
        of samples in the table, use the `nodirectory` option for at least 128
        `brr` directives.


```

Similar to how the hardware sample table (or "directory") must be page aligned ([see below](#spcasmdirectiveunaligned_sample_table)), it cannot contain more than 256 entries since an entry index is only 8 bytes. Again, spcasm can generate oversized tables just fine, but they will not work in hardware. If you need more than 256 samples or multiple sample tables, you currently have to define them yourself, but with spcasm's powerful reference system, this is not too difficult.

#### spcasm::directive::unaligned_sample_table

```trycmd
$ spcasm -w all tests/errors/sample-table-unaligned.spcasmtest
? 1
spcasm::directive::unaligned_sample_table

  × Sample table at address `0057` is not correctly aligned
   ╭─[tests/errors/sample-table-unaligned.spcasmtest:4:1]
 1 │ org $56
 2 │ db $aa
 3 │ 
 4 │ sampletable noalign
   · ─────────┬─────────
   ·          ╰── automatic sample table generated at address 0057
 5 │ brr "../yoshi.wav": 0-20
   ╰────
  help: The BRR sample table must be aligned to 256-byte-pages (lowest eight
        bits of address are zero) in order to be properly usable by hardware.
        To automatically perform this alignment, remove the `noalign` option.


```

The hardware sample table (or "directory") for BRR samples must be aligned to a page boundary. The reason for this is that the DIR DSP register, which specifies where the DSP looks up the sample table, is only 8 bits, and the lower 8 bits are hard-wired zero. spcasm is perfectly capable of generating a sample table at any address, but unless that table is aligned to a page boundary, it will not work correctly in practice.

This error only happens with the `noalign` option, as spcasm sensibly aligns the table by default. The option should only be used so you can make sure that no alignment padding is inserted, and in this case you should carefully control the position of the sample table with `org` directives or similar.

### spcasm::include_cycle

```trycmd
$ spcasm -w all tests/errors/circular-include.spcasmtest
? 1
spcasm::include_cycle

  × Include cycle detected while trying to include "tests/errors/circular-
  │ include.spcasmtest"
   ╭─[tests/errors/circular-include.spcasmtest:1:1]
 1 │ org 0
   · ▲
   · ╰── This file's inclusion causes an include cycle
 2 │ 
 3 │ include "circularly-included.spcasmtest.noerror"
   ╰────
  help: The file "tests/errors/circular-include.spcasmtest" was included:
        from tests/errors/circularly-included.spcasmtest.noerror
        from tests/errors/circular-include.spcasmtest


```

_(The example is slightly confusing; the pointer is not referring to the include directive at the beginning of the file, it's just a coincidence.)_

Assembly source file inclusion must be free of cycles, as including another file effectively copies that file's content in place of the `include` directive. This error is one of the few times when you'll see the include path of a file as it helps with troubleshooting. The file (a) that was attempted to be included is a file that directly or indirectly is a parent of the current file (b). That means that it (a) included the current file (b), which establishes a loop.

```notrycmd
                 include cycle
[file a] <-----\ detected here
   |           |
includes       |
   |        includes
   v           |
[file b] --->--/
```

Note that it is totally fine to include a file multiple times within complex include graphs. In math lingo: The (directed) include graph does not have to be a tree, but it has to be acyclic.

### spcasm::instruction

This category contains errors pertaining to instructions.

#### spcasm::instruction::invalid_addressing_mode

```trycmd
$ spcasm -w all tests/errors/addressing-mode.spcasmtest
? 1
spcasm::instruction::invalid_addressing_mode

  × Invalid addressing mode `PSW` as first operand for `MOV`
   ╭─[tests/errors/addressing-mode.spcasmtest:3:1]
 1 │ org 0
 2 │ 
 3 │ MOV PSW,Y
   · ────┬────
   ·     ╰── For this instruction
   ╰────
  help: The instruction `MOV` accepts the modes (X), (X)+, (direct_page)+Y,
        (direct_page+X), A, SP, X, Y, address, address+X, address+Y,
        direct_page, direct_page+X, direct_page+Y as first operands


```

This is one of the main instruction semantics errors. It generically means that the addressing mode in this position is not supported for this instruction. For some instructions, this error is reported even if the addressing mode would be legal paired with a different addressing mode or in a different position. This should be the domain of invalid_addressing_mode_combination, but that's not always the case.

To solve this error, it doesn't hurt to look at [the SPC700 reference](reference).

#### spcasm::instruction::invalid_constant

The given symbol constant (keyword) is invalid; this depends on the type of constant being dealt with. For example: A register was expected, but the symbol is not a register name: `A` and `X` are valid constants for registers, but `C` and `D` are not.

Note: This is a theoretical error the lexer can produce, but because invalid constants are assumed to be identifiers, this error does not currently surface to the user.

#### spcasm::instruction::missing_operand

```trycmd
$ spcasm -w all tests/errors/missing-operand.spcasmtest
? 1
spcasm::instruction::missing_operand

  × `MOV` takes at least one operand
   ╭─[tests/errors/missing-operand.spcasmtest:2:1]
 1 │ org 0
 2 │ mov
   · ─┬─
   ·  ╰── Takes at least one operand
   ╰────
  help: Add any of the operands (X), (X)+, (direct_page)+Y, (direct_page+X),
        A, SP, X, Y, address, address+X, address+Y, direct_page,
        direct_page+X, direct_page+Y to this instruction


```

Similar to [instruction::operand_not_allowed](#spcasminstructionoperandnotallowed). Some instructions instead require at least one operand.

#### spcasm::instruction::missing_second_operand

```trycmd
$ spcasm -w all tests/errors/missing-second-operand.spcasmtest
? 1
spcasm::instruction::missing_second_operand

  × `MOV` takes two operands
   ╭─[tests/errors/missing-second-operand.spcasmtest:2:1]
 1 │ org 0
 2 │ mov a
   · ──┬──
   ·   ╰── Takes two operands
   ╰────
  help: Add any of the operands #immediate, (X), (X)+, (direct_page)+Y,
        (direct_page+X), X, Y, address, address+X, address+Y, direct_page,
        direct_page+X, direct_page+Y to this instruction


```

Similar to [instruction::missing_operand](#spcasminstructionmissingoperand). Some instructions require two operands, and not just one.

#### spcasm::instruction::operand_not_allowed

```trycmd
$ spcasm -w all tests/errors/no-operand.spcasmtest
? 1
spcasm::instruction::operand_not_allowed

  × `NOP` doesn't take any operands
   ╭─[tests/errors/no-operand.spcasmtest:2:1]
 1 │ org 0
 2 │ nop a
   · ──┬──
   ·   ╰── Takes 0 operands
   ╰────
  help: Remove the operands of this instruction


```

Similar to [instruction::two_operands_not_allowed](#spcasminstructiontwooperandsnotallowed). The instruction takes no operands, but one or two were specified.

#### spcasm::instruction::two_operands_not_allowed

```trycmd
$ spcasm -w all tests/errors/two-operands.spcasmtest
? 1
spcasm::instruction::two_operands_not_allowed

  × Two operands are not allowed for `POP`
   ╭─[tests/errors/two-operands.spcasmtest:2:1]
 1 │ org 0
 2 │ pop x,a
   · ───┬───
   ·    ╰── Only takes 1 operand
   ╰────
  help: Remove the second operand


```

Similar to [instruction::operand_not_allowed](#spcasminstructionoperandnotallowed). The instruction only takes one operand/addressing mode, but two were given.

### spcasm::io

This category contains I/O related errors.

#### spcasm::io::audio_processing_error

```trycmd
$ spcasm -w all tests/errors/audio-processing.spcasmtest
? 1
spcasm::io::audio_processing_error

  × Ill-formed WAVE file: no RIFF tag found
   ╭─[tests/errors/audio-processing.spcasmtest:3:1]
 1 │ org 0
 2 │ 
 3 │ brr "../binary.bin"
   · ─────────┬─────────
   ·          ╰── While processing audio here
   ╰────
  help: This error is caused by the malformed input audio file "../
        binary.bin". If your audio player understands this file just fine,
        please file an spcasm bug.


```

The `brr` directive reads audio from WAV files and reports this error if the input file is not a valid WAV file. The exact error depends on what the third-party wav backend reports, but it is usually enough to check which file was included and test it with an audio player.

Known unsupported WAV features include all WAV extensions which compress audio, so use standard uncompressed WAV files instead.

#### spcasm::io::file_not_found

```notrycmd
$ spcasm -w all tests/errors/file-not-found.spcasmtest
? 1
Error: spcasm::io::file_not_found

  × File "tests/errors/this_definitely_does_not_exist" was not found
  ╰─▶ File not found. (os error 2)
   ╭─[tests/errors/file-not-found.spcasmtest:1:1]
 1 │ org 0
 2 │ include "this_definitely_does_not_exist"
   · ───────────────────┬───────────────────
   ·                    ╰── File was requested here
   ╰────


```

_Technically_ this is a proxy error for all kinds of I/O errors, but it almost exclusively happens when spcasm can't find, open or read another file that was requested. This includes the main source file provided on the command line, but also other source or binary files. The operating system's original error is also contained in the report.

### spcasm::reference

This category concerns itself with the use of references.

#### spcasm::reference::missing_global

```trycmd
$ spcasm -w all tests/errors/missing-global.spcasmtest
? 1
spcasm::reference::missing_global

  × There is no global label defined before the local label 'local_label'
   ╭─[tests/errors/missing-global.spcasmtest:2:2]
 1 │ org 0
 2 │ .local_label:
   ·  ─────┬─────
   ·       ╰── Local label defined here
 3 │     nop
   ╰────
  help: Add a global label before defining this local label.


```

Local labels always have a scope defined as the range between their "parent" global label and the next global label after them. The parent specifically is the global label directly before the local label. Therefore, any local label must be preceded by a global label that is its parent; a local label as the first label of a file is not allowed. An easy solution is to define a dummy label at the beginning of the file that is never used but provides an initial scope for local labels.

#### spcasm::reference::redefine

```trycmd
$ spcasm -w all tests/errors/double-define.spcasmtest
? 1
spcasm::reference::redefine

  × Reference 'here' was defined more than once
   ╭─[tests/errors/double-define.spcasmtest:3:1]
 1 │ org 0
 2 │ 
 3 │ here:
   · ──┬─
   ·   ╰── 'here' first defined here…
 4 │     nop
 5 │ here:
   · ──┬─
   ·   ╰── … and later redefined here
 6 │     bra here
   ╰────
  help: Local references are a convenient way to safely reuse common names
        like '.loop' or '.end'.


```

Defining a reference with the same name more than once is ambiguous and not allowed. Some assemblers allow this and have different behavior for which reference they actually use when; spcasm would _in theory probably_ always use the first definition in source code order. However, this is not considered a useful feature and therefore disallowed.

Some common names, such as `loop`, `end`, `return`, `continue`, `again`, `else`, ... should be usable more than once in the program without ambiguity. The [local label system](reference/README.md#labels-and-references) of spcasm and other assemblers allow you to use local labels (leading `.` before the label name) for these instead, so they only have to be unique within a global label.

Consequently, instead of this:

```asm
some_procedure:
  mov a, x
  ; ...
  loop:
    mov y, DSPDATA
    ; ...
    bne loop

some_other_procedure:
  mov y, #10
  loop:
    ; ...
    dbnz y, loop
```

where the `loop` label exists twice and will cause an error like above, do this:

```asm
some_procedure:
  mov a, x
  ; ...
  .loop:
    mov y, DSPDATA
    ; ...
    bne .loop

some_other_procedure:
  mov y, #10
  .loop:
    ; ...
    dbnz y, .loop
```

#### spcasm::reference::unresolved

```trycmd
$ spcasm -w all tests/errors/unresolved-label.spcasmtest
? 1
spcasm::reference::unresolved

  × Reference 'no_exist' can not be resolved to a value
   ╭─[tests/errors/unresolved-label.spcasmtest:2:1]
 1 │ org 0
 2 │ mov a,no_exist
   · ───────┬──────┬
   ·        │      ╰── 'no_exist' defined here
   ·        ╰── Used here
   ╰────
  help: Any symbolic reference must be defined somewhere. Did you misspell the
        reference's name?
        This error is sometimes caused by too few reference resolution passes.
        Use `--reference-pass-limit` to increase the limit on the number of
        passes.


```

Evidently, references which are not defined anywhere in the assembly source cannot be used in instructions or directives. Defining a reference involves either using it as the label for some instruction (then the memory address will be filled in automatically), or assigning it a literal value to make it act like a constant.

```assembly
label: nop ; reference has the memory address of the nop instruction
label = 50 ; reference has value 50
```

Often, however, a reference is simply misspelled.

### spcasm::segment

This category contains errors relating to segment use.

#### spcasm::segment::empty_stack

```trycmd
$ spcasm -w all tests/errors/empty-segment-stack.spcasmtest
? 1
spcasm::segment::empty_stack

  × There is no segment on the stack
   ╭─[tests/errors/empty-segment-stack.spcasmtest:3:1]
 1 │ org 0
 2 │ 
 3 │ pullpc
   · ───┬──
   ·    ╰── Segment stack access here
   ╰────
  help: Directives like `pullpc` require that you push a segment to the stack
        beforehand with `pushpc`.


```

The segment stack stores segments for later reuse and is manipulated with `pushpc` and `pullpc` Similar to [segment::missing](#spcasmsegmentmissing), you can't restore a segment if none has been saved yet.

#### spcasm::segment::mismatch

```trycmd
$ spcasm -w all tests/errors/segment-mismatch.spcasmtest
? 1
spcasm::segment::mismatch

  × Segment at 0005 starts before the end of the previous one, which is 000a
   ╭─[tests/errors/segment-mismatch.spcasmtest:1:17]
 1 │  00 01 02 03 04 [05] 06 07 08 09
   ·                 ─┬
   ·                  ╰── Unexpected
   ╰────


```

When assembling different segments together, there must not be any overlap between their data. This is usually the result of starting a segment over in the wrong place; the `pushpc` and `pullpc` specifically allow you to come back to a segment after dealing with different segment(s) in between.

#### spcasm::segment::missing

```trycmd
$ spcasm -w all tests/errors/missing-segment-after-pop.spcasmtest
? 1
spcasm::segment::missing

  × There is no active segment here
   ╭─[tests/errors/missing-segment-after-pop.spcasmtest:3:1]
 1 │ org 0
 2 │ pushpc
 3 │ db 0
   · ──┬─
   ·   ╰── This requires that there be a segment
   ╰────
  help: Start a new segment with `org <memory address>`


```

Segments specify where the currently-being-assembled binary data belongs in the address space. In a variety of situations, the assembler has no active segment, like after saving the current segment to the stack. You now need to provide a new segment, for example with an `org` directive or by restoring the saved segment from the stack with `pullpc`.

### spcasm::syntax

This category contains syntax errors.

#### spcasm::syntax::expected_token

```trycmd
$ spcasm -w all tests/errors/parser-dangling.spcasmtest
? 1
spcasm::syntax::expected_token

  × Expected any of "/n", ";="
   ╭─[tests/errors/parser-dangling.spcasmtest:3:8]
 1 │ org 0
 2 │ adc a,x
 3 │ endasm 50
   ·        ─┬
   ·         ╰── This number is invalid here
   ╰────


```

The most common type of syntax error. "Token" in programming language lingo is the smallest sensible unit of text in the source code, which in this case was invalid. The parser expected a different token than what was actually found. A list of possible tokens (sometimes just one) is given by the parser, which hopefully helps you to discover the syntactical problem. In the above, for example, we can clearly see the parser expects, among other things, a directive, or a mnemonic, i.e. the start of an instruction.

#### spcasm::syntax::invalid_bit_index

```trycmd
$ spcasm -w all tests/errors/parser-bit-index.spcasmtest
? 1
spcasm::syntax::invalid_bit_index

  × Invalid bit index `8`
   ╭─[tests/errors/parser-bit-index.spcasmtest:2:9]
 1 │ org 0
 2 │ set1 33.8
   ·         ┬
   ·         ╰── Bit index is invalid
   ╰────
  help: Use a bit index between 0 and 7 inclusive


```

Some addressing modes contain an intra-byte bit index; this has to be between 0 and 7 inclusive as there are 8 bits to a byte.

#### spcasm::syntax::invalid_number

```trycmd
$ spcasm -w all tests/errors/parser-invalid-number.spcasmtest
? 1
spcasm::syntax::invalid_number

  × Invalid number: invalid digit found in string
   ╭─[tests/errors/parser-invalid-number.spcasmtest:2:9]
 1 │ org 0
 2 │ label = %134af
   ·         ───┬──
   ·            ╰── invalid digit found in string
   ╰────


```

```trycmd
$ spcasm -w all tests/errors/parser-too-large-number.spcasmtest
? 1
spcasm::syntax::invalid_number

  × Invalid number: number too large to fit in target type
   ╭─[tests/errors/parser-too-large-number.spcasmtest:1:5]
 1 │ org $99999999999999999999999999999999999999
   ·     ───────────────────┬───────────────────
   ·                        ╰── number too large to fit in target type
   ╰────


```

Numbers need to conform to a specific format that also depends on the radix, e.g. whether you're specifying numbers in hexadecimal or binary. The exact error, two different examples shown above, gives you a good clue of what is wrong with this number.

#### spcasm::syntax::invalid_test_comment

```notrycmd
  × Test comment has invalid format
   ╭─[examples/errors/parser-test-comment.spcasm:1:1]
 1 │ nop ;= ajsdkfl
   ·     ────┬───
   ·         ╰── This ';=' comment is invalid: invalid digit found in string
   ╰────
  help: Test comments consist of a series of space-delimited bytes, given as hexadecimal, for example `;= 0F AA B8` for three bytes
```

Test comments are only used in spcasm's testing system, so you won't see this error unless you're developing spcasm and writing tests. The specific test comment format is a sequence of space-delimited bytes given in hexadecimal.

#### spcasm::syntax::missing_token

```trycmd
$ spcasm -w all tests/errors/parser-missing-token.spcasmtest
? 1
spcasm::syntax::missing_token

  × Expected any of "identifier", "mnemonic", "org", "db", "byte", "dw",
  │ "word", "dl", "dd", "ascii", "asciiz", "incbin", "include", "incsrc",
  │ "endasm", "brr", "sampletable", "pushpc", "pullpc", "arch", "repeatcount",
  │ "startpos", "fill", "fillbyte", "fillword", "filllong", "filldword",
  │ "pad", "padbyte", "padword", "padlong", "paddword", "macro", "if", "math",
  │ "namespace", "repeat", "+", "+++", "---", "-", "-range-", "<", "%", ".",
  │ "/n"
   ╭─[tests/errors/parser-missing-token.spcasmtest:3:1]
 1 │ org 0
 2 │ label:
   ╰────


```

A similar error to [expected_token](#spcasmsyntaxexpectedtoken), though in this case the file ends without some token that needs to occur in order for the file to be syntactically valid. The error message is intentionally similar to expected_token as there mostly is no difference from the user's perspective.

#### spcasm::syntax::unexpected_character

```trycmd
$ spcasm -w all tests/errors/parser-unexpected-char.spcasmtest
? 1
spcasm::syntax::unexpected_character

  × Unexpected character "ö"
   ╭─[tests/errors/parser-unexpected-char.spcasmtest:1:1]
 1 │ ö
   · ▲
   · ╰── Unexpected
   ╰────


```

A generic early error when spcasm can't at all make sense of some character in the assembly source.

### spcasm::user_macro

This category concerns itself with the definition and usage of user-defined macros.

#### spcasm::user_macro::argument_outside_macro

```notrycmd

```

Macro arguments are specific to user-defined macros and cannot be used in any way outside of them.

Note: This error is theoretical, macro arguments currently cause undefined reference errors.

#### spcasm::reference::assign_invalid

```trycmd
$ spcasm -w all tests/errors/assign-to-argument.spcasmtest
? 1
spcasm::reference::assign_invalid

  × Assigning a value to macro argument '<first>' is not possible
   ╭─[tests/errors/assign-to-argument.spcasmtest:4:2]
 1 │ org 0
 2 │ 
 3 │ macro my_macro(first)
 4 │     <first> = $40
   ·     ──────┬──────
   ·           ╰── Assignment happens here
 5 │ endmacro
 6 │ 
 7 │ %my_macro(5)
   ╰────
  help: Arguments of macros are given a value when the macro is called.
        Therefore, it does not make sense to assign them a value. If you need
        a label with a specific value inside a macro, use a local label under
        the macro's special '/@' label instead


```

Some references cannot be assigned a value directly, though the reasons vary:

- Arguments to user-defined macros are specified with angle bracket syntax, like `<argument>`. When the macro is called, the arguments are replaced with the value given in the macro call. Therefore, it does not make sense to manually assign a specific value to the argument, like you can do with other kinds of labels. Instead, you can for example use local labels under the macro's special `\@` label.
- Relative labels (`+`/`-`) cannot be assigned a value, since that simply does not make sense. They are addressed by relative labels in instructions, and using them in an assignment does not properly give them any relative position, since assigned references are considered to have no position at all. (That's also why assigned global references do not start a new "scope" of local references.)

#### spcasm::user_macro::incorrect_number_of_arguments

```trycmd
$ spcasm -w all tests/errors/too-few-arguments.spcasmtest
? 1
spcasm::user_macro::incorrect_number_of_arguments

  × Macro 'my_macro' takes 1 arguments, but 0 were supplied
   ╭─[tests/errors/too-few-arguments.spcasmtest:3:1]
 1 │     org 0
 2 │     
 3 │ ╭─▶ macro my_macro(one)
 4 │ ├─▶ endmacro
   · ╰──── 'my_macro' defined here with 1 arguments
 5 │     
 6 │     %my_macro()
   ·     ─────┬─────
   ·          ╰── In this macro call
   ╰────
  help: Add arguments


```

Any macro must be called exactly with the number of arguments that it was defined with. There are no variable arguments or default arguments in spcasm; you can define alternate differently-named versions of the same macro if you need such features.

#### spcasm::user_macro::recursive_definition

```trycmd
$ spcasm -w all tests/errors/recursive-definition.spcasmtest
? 1
spcasm::user_macro::recursive_definition

  × User macro 'inner' is defined inside another user macro
   ╭─[tests/errors/recursive-definition.spcasmtest:3:7]
 1 │ org 0
 2 │ 
 3 │ macro outer
   ·       ──┬──
   ·         ╰── Outer macro defined here
 4 │   mov a,#4
 5 │   macro inner
 6 │     nop
   ·     ─┬─
   ·      ╰── Inner macro defined here
 7 │   endmacro
 8 │ endmacro
   ╰────
  help: User-defined macros can only be defined at the top level of a file, as
        inner definition does not make sense. If you want to avoid name
        collisions, use a more distinctive name.


```

A user-defined macro cannot be defined within another user-defined macro. That simply makes no sense. A common reason as to why you would want to do that is to avoid name collisions. The only way to do this, however, is to choose a non-colliding name, for example by using C-style prefixes.

#### spcasm::user_macro::undefined

```trycmd
$ spcasm -w all tests/errors/undefined-macro.spcasmtest
? 1
spcasm::user_macro::undefined

  × Macro 'does_not_exist' is not defined
   ╭─[tests/errors/undefined-macro.spcasmtest:6:1]
 3 │ macro does_ot_exist
 4 │ endmacro
 5 │ 
 6 │ %does_not_exist()
   · ────────┬────────
   ·         ╰── Macro used here
   ╰────
  help: The available macros are: 'does_ot_exist'.


```

Evidently, a macro must be defined with the exact name of its usage. Note that spcasm does not require you to define a macro before using it, and it may also live in another source file which is included.

#### spcasm::user_macro::undefined_argument

```trycmd
$ spcasm -w all tests/errors/undefined-macro-argument.spcasmtest
? 1
spcasm::user_macro::undefined_argument

  × Macro argument 'two' has not been defined in this macro
   ╭─[tests/errors/undefined-macro-argument.spcasmtest:4:9]
 1 │ org 0
 2 │ 
 3 │ macro my_macro(one)
 4 │     mov a,#<two>
   ·            ──┬─
   ·              ╰── Macro argument used here
 5 │ endmacro
   ╰────
  help: The available arguments are: 'one'. Did you misspell the macro
        argument's name?


```

You can only use macro arguments of the current macro with the exact names that they were defined with. It is not possible to access arguments of macros that this macro is being called inside; in general the macro may be called from anywhere and if you need access to these arguments, you can pass them along from the outer to the inner macro via another argument. Remember that macro arguments are only a compile-time thing and incur no cost at runtime :).
