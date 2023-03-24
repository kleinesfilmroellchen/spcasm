# Error reference

spcasm reports various kinds of errors, warnings and advice. Errors always abort the assembly process. Warnings inform you of things that might not have the behavior you want, and point out what the assembler actually did. Advice, often called "Infos" in IDEs, informs you of assembler behavior that you are probably expecing, but which is nice to know about anyways.

All of these three are often collectively called errors, as they can be turned into hard errors with the right settings (see [Usage](usage.md)). Each error can be uniquely identified by its error code, which starts with `spcasm::` by convention and follows a Rust namespace-like syntax. The various categories group errors related to common spcasm features. (There are no numeric error codes, not even behind the scenes; spcasm always exits with exit code 1 if an error occurs.)

In the following, each error is listed with an example and a detailed explanation of what the problem is and how to deal with it. Note that the exact reports in the examples are not always updated and might not reflect what spcasm currently reports, but the error code is usually stable.

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
$ spcasm tests/opcodes.s
spcasm::arch::valid

  ☞ Legal architecture directive ignored
   ╭─[tests/opcodes.s:1:1]
 1 │ arch spc700-raw
   · ───────┬───────
   ·        ╰── `arch` directive
 2 │ org 0
 3 │ start:        ; @ 0
 4 │    MOV A,#$10    ;= E8 10
   ╰────
  help: spcasm supports `arch` directives for compatibility with the Asar
        multi-architecture assembler. This arch directive points to the
        spc700 architecture and is therefore safely ignored.


```

spcasm has a whole collection of features targeted at compatibility with the popular [Asar](https://rpghacker.github.io/asar/manual/) multi-architecture SNES patch assembler. As a multi-architecture assembler, Asar allows you to [specify the architecture of an assembly file](https://rpghacker.github.io/asar/manual/#archs) and even switch between them. In order to allow compilation with pure SPC-700 assembly originally written for Asar, spcasm therefore recognizes the `arch` directive and accepts any SPC-700-related architecture specifications like `spc700`. This advice exists so that you are informed of ignored architecture directives and to allow you to forbid the use of such directives.

## Warnings

### spcasm::value_too_large

```trycmd
$ spcasm tests/parse.spcasmtest

```

In most places, numbers have a limited size, often 8 or 16 bits. However, user-specified numbers are initially limited much higher, to 64-bit signed numbers, in order to allow computations that involve large intermediate values. When using those large numbers in these bit-limited places, the higher bits are simply cut off or truncated. This can be surprising depending on what exact instruction or directive the number is used in, so spcasm issues a specific warning by default.

## Errors

### spcasm::arch::invalid

```trycmd
$ spcasm tests/errors/unsupported-arch.spcasmtest
? 1
Error: spcasm::arch::invalid

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

#### spcasm::directive::invalid_directive_option

```trycmd
$ spcasm tests/errors/invalid-brr-option.spcasmtest
? 1
Error: spcasm::directive::invalid_directive_option

  × Invalid option `wrongoption` for directive `brr`
   ╭─[tests/errors/invalid-brr-option.spcasmtest:1:1]
 1 │ brr "../song.wav" wrongoption
   · ────────┬──────── ─────┬─────
   ·         │              ╰── Invalid option specified here
   ·         ╰── `brr` directive defined here
   ╰────
  help: The valid options are `nodirectory`, `autotrim`


```

Some directives accept options as identifiers, but the one you used was not one of them. Refer to the [directive documentation](reference/directives.md) to learn more about supported options for these directives.

#### spcasm::directive::invalid_range

```trycmd
$ spcasm tests/errors/parser-invalid-range.spcasmtest
? 1
Error: spcasm::directive::invalid_range

  × 4 is greater than 2
   ╭─[tests/errors/parser-invalid-range.spcasmtest:1:1]
 1 │ org 0
 2 │ incbin "../binary.bin":4-2
   ·                       ─┬
   ·                        ╰── In this range
   ╰────
  help: Switch the range limits around: `2-4`


```

For range specifications, like when including binary files, the Asar style range syntax is a `start-end` format. Obviously, the start then needs to be before (or the same as) the end. Often you just accidentally swapped these limits.

#### spcasm::math_pri_unsupported

```trycmd
$ spcasm tests/errors/math-pri.spcasmtest
? 1
Error: spcasm::directive::math_pri_unsupported

  × `math pri` is not supported by spcasm
   ╭─[tests/errors/math-pri.spcasmtest:1:1]
 1 │ 
 2 │ math pri off
   · ──┬─
   ·   ╰── `math pri` directive
   ╰────
  help: This directive switches between normal operator precedence and
        "dumb" left-to-right precedence in the Asar multi-architecture
        assembler. This feature is intended for xkas compatibility, but
        spcasm does not consider this an important feature. Therefore,
        spcasm cannot switch math priority mode. Please check your math
        expressions manually if you are using `math pri off`.


```

`math pri` is a directive that Asar supports for xkas compatibility. By default, Asar operates in left-to-right math priority like xkas, and proper priority can be enabled with `math pri on`. spcasm does not have this feature, however, to allow easier switching over to spcasm, the directive is detected and an error is reported. There are two courses of action:

- If your assembly entirely or mostly uses `math pri on`, you can just remove the directive, as spcasm uses proper math priority like Asar with `math pri on`.
- If your assembly entirely or mostly uses `math pri off`, you can bracket your math expressions manually to emulate the left-to-right precedence behavior of xkas. Then, remove the directive.

#### spcasm::directive::range_out_of_bounds

```trycmd
$ spcasm tests/errors/range-out-of-bounds.spcasmtest
? 1
Error: spcasm::directive::range_out_of_bounds

  × The range 20000-50000 is out of bounds for the input file "../binary.bin"
   ╭─[tests/errors/range-out-of-bounds.spcasmtest:1:1]
 1 │ org 0
 2 │ incbin "../binary.bin":20000-50000
   · ────────────────┬────────────────
   ·                 ╰── Out of bounds range defined here
   ╰────
  help: The input's length is 6


```

When providing a range for a binary file that is included, the range is allowed to exceed the end of the binary, at which point just everything until the binary's end is included. However, the start point must lie within the binary.

#### spcasm::directive::references_as_argument

```trycmd
$ spcasm tests/errors/label-in-org.spcasmtest
? 1
Error: spcasm::directive::references_as_argument

  × Invalid use of labels in an argument for `org`
   ╭─[tests/errors/label-in-org.spcasmtest:3:1]
 3 │ dp_label:
 4 │    nop       ; dummy
 5 │ 
 6 │ org dp_label
   · ─┬─
   ·  ╰── This directive
   ╰────
  help: Because the directive argument can determine a reference's position,
        resolving the argument value is not generally possible. For this
        reason, references are not allowed to be used in a directive
        argument.


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

### spcasm::directive::sample_table_too_large

```trycmd
$ spcasm tests/errors/sample-table-too-large.spcasmtest
? 1
Error: spcasm::directive::sample_table_too_large

  × BRR sample table has more than 256 entries
   ╭─[tests/errors/sample-table-too-large.spcasmtest:1:1]
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
        autogenerated table contains all BRR samples without the
        `nodirectory` option, which includes 384 samples in this case. To
        reduce the number of samples in the table, use the `nodirectory`
        option for at least 128 `brr` directives.


```

Similar to how the hardware sample table (or "directory") must be page aligned ([see below](#spcasmdirectiveunaligned_sample_table)), it cannot contain more than 256 entries since an entry index is only 8 bytes. Again, spcasm can generate oversized tables just fine, but they will not work in hardware. If you need more than 256 samples or multiple sample tables, you currently have to define them yourself, but with spcasm's powerful reference system, this is not too difficult.

#### spcasm::directive::unaligned_sample_table

```trycmd
$ spcasm tests/errors/sample-table-unaligned.spcasmtest
? 1
Error: spcasm::directive::unaligned_sample_table

  × Sample table at address 0057 is not correctly aligned
   ╭─[tests/errors/sample-table-unaligned.spcasmtest:1:1]
 1 │ org $56
 2 │ db $aa
 3 │ 
 4 │ sampletable noalign
   · ─────────┬─────────
   ·          ╰── automatic sample table generated at address 0057
 5 │ brr "../yoshi.wav": 0-20
   ╰────
  help: The BRR sample table must be aligned to 256-byte-pages (lowest
        eight bits of address are zero) in order to be properly usable
        by hardware. To automatically perform this alignment, remove the
        `noalign` option.


```

The hardware sample table (or "directory") for BRR samples must be aligned to a page boundary. The reason for this is that the DIR DSP register, which specifies where the DSP looks up the sample table, is only 8 bits, and the lower 8 bits are hard-wired zero. spcasm is perfectly capable of generating a sample table at any address, but unless that table is aligned to a page boundary, it will not work correctly in practice.

This error only happens with the `noalign` option, as spcasm sensibly aligns the table by default. The option should only be used so you can make sure that no alignment padding is inserted, and in this case you should carefully control the position of the sample table with `org` directives or similar.

### spcasm::include_cycle

```trycmd
$ spcasm tests/errors/circular-include.spcasmtest
? 1
Error: spcasm::include_cycle

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
$ spcasm tests/errors/addressing-mode.spcasmtest
? 1
Error: spcasm::instruction::invalid_addressing_mode

  × Invalid addressing mode `PSW` as first operand for `MOV`
   ╭─[tests/errors/addressing-mode.spcasmtest:1:1]
 1 │ org 0
 2 │ 
 3 │ MOV PSW,Y
   · ────┬───
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
$ spcasm tests/errors/missing-operand.spcasmtest
? 1
Error: spcasm::instruction::missing_operand

  × `MOV` takes at least one operand
   ╭─[tests/errors/missing-operand.spcasmtest:1:1]
 1 │ org 0
 2 │ mov
   · ─┬
   ·  ╰── Takes at least one operand
   ╰────
  help: Add any of the operands (X), (X)+, (direct_page)+Y, (direct_page+X),
        A, SP, X, Y, address, address+X, address+Y, direct_page,
        direct_page+X, direct_page+Y to this instruction


```

Similar to [instruction::operand_not_allowed](#spcasminstructionoperandnotallowed). Some instructions instead require at least one operand.

#### spcasm::instruction::missing_second_operand

```trycmd
$ spcasm tests/errors/missing-second-operand.spcasmtest
? 1
Error: spcasm::instruction::missing_second_operand

  × `MOV` takes two operands
   ╭─[tests/errors/missing-second-operand.spcasmtest:1:1]
 1 │ org 0
 2 │ mov a
   · ──┬─
   ·   ╰── Takes two operands
   ╰────
  help: Add any of the operands #immediate, (X), (X)+, (direct_page)+Y,
        (direct_page+X), X, Y, address, address+X, address+Y, direct_page,
        direct_page+X to this instruction


```

Similar to [instruction::missing_operand](#spcasminstructionmissingoperand). Some instructions require two operands, and not just one.

#### spcasm::instruction::operand_not_allowed

```trycmd
$ spcasm tests/errors/no-operand.spcasmtest
? 1
Error: spcasm::instruction::operand_not_allowed

  × `NOP` doesn't take any operands
   ╭─[tests/errors/no-operand.spcasmtest:1:1]
 1 │ org 0
 2 │ nop a
   · ──┬─
   ·   ╰── Takes 0 operands
   ╰────
  help: Remove the operands of this instruction


```

Similar to [instruction::two_operands_not_allowed](#spcasminstructiontwooperandsnotallowed). The instruction takes no operands, but one or two were specified.

#### spcasm::instruction::two_operands_not_allowed

```trycmd
$ spcasm tests/errors/two-operands.spcasmtest
? 1
Error: spcasm::instruction::two_operands_not_allowed

  × Two operands are not allowed for `POP`
   ╭─[tests/errors/two-operands.spcasmtest:1:1]
 1 │ org 0
 2 │ pop x,a
   · ───┬──
   ·    ╰── Only takes 1 operand
   ╰────
  help: Remove the second operand


```

Similar to [instruction::operand_not_allowed](#spcasminstructionoperandnotallowed). The instruction only takes one operand/addressing mode, but two were given.

### spcasm::io

This category contains I/O related errors.

#### spcasm::io::audio_processing_error

```trycmd
$ spcasm tests/errors/audio-processing.spcasmtest
? 1
Error: spcasm::io::audio_processing_error

  × failed to fill whole buffer
   ╭─[tests/errors/audio-processing.spcasmtest:1:1]
 1 │ org 0
 2 │ 
 3 │ brr "../binary.bin"
   · ─────────┬────────
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
$ spcasm tests/errors/file-not-found.spcasmtest
? 1
Error: spcasm::io::file_not_found

  × File "tests/errors/this_definitely_does_not_exist" was not found
  ╰─▶ Das System kann die angegebene Datei nicht finden. (os error 2)
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
$ spcasm tests/errors/missing-global.spcasmtest
? 1
Error: spcasm::reference::missing_global

  × There is no global label defined before the local label 'local_label'
   ╭─[tests/errors/missing-global.spcasmtest:1:1]
 1 │ org 0
 2 │ .local_label:
   ·  ─────┬─────
   ·       ╰── Local label defined here
 3 │     nop
   ╰────
  help: Add a global label before defining this local label


```

Local labels always have a scope defined as the range between their "parent" global label and the next global label after them. The parent specifically is the global label directly before the local label. Therefore, any local label must be preceded by a global label that is its parent; a local label as the first label of a file is not allowed. An easy solution is to define a dummy label at the beginning of the file that is never used but provides an initial scope for local labels.

#### spcasm::reference::unresolved

```trycmd
$ spcasm tests/errors/unresolved-label.spcasmtest
? 1
Error: spcasm::reference::unresolved

  × Reference 'no_exist' can not be resolved to a value
   ╭─[tests/errors/unresolved-label.spcasmtest:1:1]
 1 │ org 0
 2 │ mov a,no_exist
   · ──────┬──────┬
   ·       │      ╰── 'no_exist' defined here
   ·       ╰── Used here
   ╰────
  help: Any symbolic reference must be defined somewhere. Did you misspell
        the reference's name?
        This error is sometimes caused by too few reference resolution
        passes. Use `--reference-pass-limit` to increase the limit on the
        number of passes.


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
$ spcasm tests/errors/empty-segment-stack.spcasmtest
? 1
Error: spcasm::segment::empty_stack

  × There is no segment on the stack
   ╭─[tests/errors/empty-segment-stack.spcasmtest:1:1]
 1 │ org 0
 2 │ 
 3 │ pullpc
   · ──┬──
   ·   ╰── Segment stack access here
   ╰────
  help: Directives like `pullpc` require that you push a segment to the
        stack beforehand with `pushpc`.


```

The segment stack stores segments for later reuse and is manipulated with `pushpc` and `pullpc` Similar to [segment::missing](#spcasmsegmentmissing), you can't restore a segment if none has been saved yet.

#### spcasm::segment::mismatch

```trycmd
$ spcasm tests/errors/segment-mismatch.spcasmtest
? 1
Error: spcasm::segment::mismatch

  × Segment at 0005 starts before the end of the previous one, which is 000a
   ╭─[tests/errors/segment-mismatch.spcasmtest:1:1]
 1 │  00 01 02 03 04 [05] 06 07 08 09
   ·                 ─┬
   ·                  ╰── Unexpected
   ╰────


```

When assembling different segments together, there must not be any overlap between their data. This is usually the result of starting a segment over in the wrong place; the `pushpc` and `pullpc` specifically allow you to come back to a segment after dealing with different segment(s) in between.

#### spcasm::segment::missing

```trycmd
$ spcasm tests/errors/missing-segment-after-pop.spcasmtest
? 1
Error: spcasm::segment::missing

  × There is no active segment here
   ╭─[tests/errors/missing-segment-after-pop.spcasmtest:1:1]
 1 │ org 0
 2 │ pushpc
 3 │ db 0
   · ─┬─
   ·  ╰── This requires that there be a segment
   ╰────
  help: Start a new segment with `org <memory address>`


```

Segments specify where the currently-being-assembled binary data belongs in the address space. In a variety of situations, the assembler has no active segment, like after saving the current segment to the stack. You now need to provide a new segment, for example with an `org` directive or by restoring the saved segment from the stack with `pullpc`.

### spcasm::syntax

This category contains syntax errors.

#### spcasm::syntax::dangling_tokens

A syntax error issued when there are unconsumed tokens not used by the parser at the end of the file.

Note: This is a theoretical error forwarded from the parser backend. It is not known whether this error can actually be produced by spcasm's parser.

#### spcasm::syntax::expected_token

```trycmd
$ spcasm tests/errors/parser-dangling.spcasmtest
? 1
Error: spcasm::syntax::expected_token

  × Expected any of "/n", ";="
   ╭─[tests/errors/parser-dangling.spcasmtest:1:1]
 1 │ org 0
 2 │ adc a,x
 3 │ end 50
   ·     ─┬
   ·      ╰── This number is invalid here
   ╰────


```

The most common type of syntax error. "Token" in programming language lingo is the smallest sensible unit of text in the source code, which in this case was invalid. The parser expected a different token than what was actually found. A list of possible tokens (sometimes just one) is given by the parser, which hopefully helps you to discover the syntactical problem. In the above, for example, we can clearly see the parser expects, among other things, a directive, or a mnemonic, i.e. the start of an instruction.

#### spcasm::syntax::invalid_bit_index

```trycmd
$ spcasm tests/errors/parser-bit-index.spcasmtest
? 1
Error: spcasm::syntax::invalid_bit_index

  × Invalid bit index `8`
   ╭─[tests/errors/parser-bit-index.spcasmtest:1:1]
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
$ spcasm tests/errors/parser-invalid-number.spcasmtest
? 1
Error: spcasm::syntax::invalid_number

  × Invalid number: invalid digit found in string
   ╭─[tests/errors/parser-invalid-number.spcasmtest:1:1]
 1 │ org 0
 2 │ label = %34af
   ·          ──┬─
   ·            ╰── invalid digit found in string
   ╰────


```

```trycmd
$ spcasm tests/errors/parser-too-large-number.spcasmtest
? 1
Error: spcasm::syntax::invalid_number

  × Invalid number: number too large to fit in target type
   ╭─[tests/errors/parser-too-large-number.spcasmtest:1:1]
 1 │ org 99999999999999999999999999999999999999
   ·     ───────────────────┬──────────────────
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
$ spcasm tests/errors/parser-missing-token.spcasmtest
? 1
Error: spcasm::syntax::missing_token

  × Expected any of "/n", "%", ".", "<", "arch", "ascii", "asciiz", "brr",
  │ "byte", "db", "dd", "dl", "dw", "end", "fill", "fillbyte", "filldword",
  │ "filllong", "fillword", "identifier", "incbin", "include", "macro",
  │ "math", "mnemonic", "org", "padbyte", "paddword", "padlong", "padword",
  │ "pullpc", "pushpc", "sampletable", "word"
   ╭─[tests/errors/parser-missing-token.spcasmtest:1:1]
 1 │ org 0
 2 │ label:
   ╰────


```

A similar error to [expected_token](#spcasmsyntaxexpectedtoken), though in this case the file ends without some token that needs to occur in order for the file to be syntactically valid. The error message is intentionally similar to expected_token as there mostly is no difference from the user's perspective.

#### spcasm::syntax::unexpected_character

```trycmd
$ spcasm tests/errors/parser-unexpected-char.spcasmtest
? 1
Error: spcasm::syntax::unexpected_character

  × Unexpected character ö
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

#### spcasm::user_macro::assign_to_argument

```trycmd
$ spcasm tests/errors/assign-to-argument.spcasmtest
? 1
Error: spcasm::user_macro::assign_to_argument

  × Assigning a value to the macro argument '<first>' is not possible
   ╭─[tests/errors/assign-to-argument.spcasmtest:1:1]
 1 │ org 0
 2 │ 
 3 │ macro my_macro(first)
 4 │     <first> = $40
   ·     ───┬──
   ·        ╰── Assignment happens here
 5 │ endmacro
 6 │ 
 7 │ %my_macro(5)
   ╰────
  help: Arguments of macros are given a value when the macro is called.
        Therefore, it does not make sense to assign them a value. If you
        need a label with a specific value inside a macro, use a local label
        under the macro's special '/@' label instead


```

Arguments to user-defined macros are specified with angle bracket syntax, like `<argument>`. When the macro is called, the arguments are replaced with the value given in the macro call. Therefore, it does not make sense to manually assign a specific value to the argument, like you can do with other kinds of labels. Instead, you can for example use local labels under the macro's special `\@` label.

#### spcasm::user_macro::assign_to_global

```notrycmd

```

The special macro label `\@` is only designed to be used as the label for a particular instruction, so that you get a new local label scope for each macro invocation. Therefore, it is not possible to assign it a value directly.

#### spcasm::user_macro::incorrect_number_of_arguments

```trycmd
$ spcasm tests/errors/too-few-arguments.spcasmtest
? 1
Error: spcasm::user_macro::incorrect_number_of_arguments

  × Macro 'my_macro' takes 1 arguments, but 0 were supplied
   ╭─[tests/errors/too-few-arguments.spcasmtest:1:1]
 1 │     org 0
 2 │     
 3 │ ╭─▶ macro my_macro(one)
 4 │ ├─▶ endmacro
   · ╰──── 'my_macro' defined here with 1 arguments
 5 │     
 6 │ ╭─▶ %my_macro()
   · │ ─────┬─────
   · │      ╰── In this macro call
   ╰────
  help: Add arguments


```

Any macro must be called exactly with the number of arguments that it was defined with. There are no variable arguments or default arguments in spcasm; you can define alternate differently-named versions of the same macro if you need such features.

#### spcasm::user_macro::recursive_definition

```trycmd
$ spcasm tests/errors/recursive-definition.spcasmtest
? 1
Error: spcasm::user_macro::recursive_definition

  × User macro 'inner' is defined inside another user macro
   ╭─[tests/errors/recursive-definition.spcasmtest:1:1]
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
  help: User-defined macros can only be defined at the top level of a file,
        as inner definition does not make sense. If you want to avoid name
        collisions, use a more distinctive name.


```

A user-defined macro cannot be defined within another user-defined macro. That simply makes no sense. A common reason as to why you would want to do that is to avoid name collisions. The only way to do this, however, is to choose a non-colliding name, for example by using C-style prefixes.

#### spcasm::user_macro::recursive_use

```trycmd
$ spcasm tests/errors/recursive-macro.spcasmtest
? 1
Error: spcasm::user_macro::recursive_use

  × Maximum recursion depth 1000 was exceeded while expanding user macro
  │ 'recursive'
   ╭─[tests/errors/recursive-macro.spcasmtest:2:1]
 2 │ 
 3 │ macro recursive
 4 │   mov a,#4
 5 │   %recursive()
   ·   ──────┬─────
   ·         ╰── While trying to expand this macro
 6 │ endmacro
 7 │ 
 8 │ recursion_start:
   ╰────
  help: This is most likely caused by an infinitely recursive macro
        definition. On the command line, use `--macro-recursion-limit` to
        increase the limit.


```

Macros may be called within other macros, but they must at some point fully resolve to non-macro data. This may not happen if the macro is self-recursive infinitely. Because the recursion is hard to detect in an abstract manner, spcasm employs a recursion depth limit that will stop expanding macros after some point. This limit is very large by default and should suffice for all reasonable situations. However, if you _really_ need to recurse more, and your recursion is in fact finite, you can change this limit on the command line with `--macro-recursion-limit`.

#### spcasm::user_macro::undefined

```trycmd
$ spcasm tests/errors/undefined-macro.spcasmtest
? 1
Error: spcasm::user_macro::undefined

  × Macro 'does_not_exist' is not defined
   ╭─[tests/errors/undefined-macro.spcasmtest:3:1]
 3 │ macro does_ot_exist
 4 │ endmacro
 5 │ 
 6 │ %does_not_exist()
   · ───────┬───────
   ·        ╰── Macro used here
   ╰────
  help: The available macros are: 'does_ot_exist'.


```

Evidently, a macro must be defined with the exact name of its usage. Note that spcasm does not require you to define a macro before using it, and it may also live in another source file which is included.

#### spcasm::user_macro::undefined_argument

```trycmd
$ spcasm tests/errors/undefined-macro-argument.spcasmtest
? 1
Error: spcasm::user_macro::undefined_argument

  × Macro argument 'two' has not been defined in this macro
   ╭─[tests/errors/undefined-macro-argument.spcasmtest:1:1]
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
