# Error reference

spcasm reports various kinds of errors, warnings and advice. Errors always abort the assembly process. Warnings inform you of things that might not have the behavior you want, and point out what the assembler actually did. Advice, often called "Infos" in IDEs, informs you of assembler behavior that you are probably expecing, but which is nice to know about anyways.

All of these three are often collectively called errors, as they can be turned into hard errors with the right settings (see [Usage](usage.md)). Each error can be uniquely identified by its error code, which starts with `spcasm::` by convention and follows a Rust namespace-like syntax. (There are no numeric error codes, not even behind the scenes; spcasm always exits with exit code 1 if an error occurs.)

In the following, each error is listed with an example and a detailed explanation of what the problem is and how to deal with it. Note that the exact reports in the examples are not always updated and might not reflect what spcasm currently reports, but the error code is usually stable.

## Advice

### spcasm::non_direct_page_reference

```
  ☞ This reference "data" has an 8-bit value, did you want to use it in direct page addressing?
    ╭─[examples/test.spcasm:10:1]
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

Many SPC700 instructions can save space and clock cycles by using direct page addressing when accessing either the zeroth or first page of memory (see [the reference](reference/)). However, because direct page addresses depend on which of the two direct pages are selected and how large instructions actually are, spcasm can't currently automatically use direct page addressing if references appear in an operand that _might_ be a direct page address mode. Therefore, spcasm warns you that it found out this address is within a direct page, but it couldn't actually assemble the instruction to use direct-page addressing. The mnemonic suffix `.b` is available to force any instruction to use direct page addressing if possible.

### spcasm::valid_arch_macro

```
  ☞ Legal architecture macro ignored
   ╭─[examples/test.spcasm:1:1]
 1 │ arch spc700
   · ─────┬─────
   ·      ╰── `arch` macro
 2 │ org 0
 3 │ start:        ; @ 0
 4 │    MOV A,#$10    ;= E8 10
   ╰────
  help: spcasm supports `arch` macros for compatibility with the Asar multi-architecture assembler. This arch directive points to the spc700 architecture and is therefore safely ignored.
```

spcasm has a whole collection of features targeted at compatibility with the popular [Asar](https://rpghacker.github.io/asar/manual/) multi-architecture SNES patch assembler. As a multi-architecture assembler, Asar allows you to [specify the architecture of an assembly file](https://rpghacker.github.io/asar/manual/#archs) and even switch between them. In order to allow compilation with pure SPC-700 assembly originally written for Asar, spcasm therefore recognizes the `arch` macro and accepts any SPC-700-related architecture specifications like `spc700`. This advice exists so that you are informed of ignored architecture macros and to allow you to forbid the use of such macros.

## Warnings

### spcasm::value_too_large

```
  ⚠ The value ABCD is being used as a 8-bit operand here, but it is larger than this. The extra upper bits are truncated.
    ╭─[examples/parse.spcasm:17:1]
 17 │ mov A,bytelabel
 18 │
 19 │ mov a,$abcd    ;= E5 CD AB
 20 │ mov.b a,$abcd  ;= E4 CD
    · ───────────┬───────────
    ·            ╰── 8-bit operand
 21 │
 22 │ dw 40, 30, 20,10
 23 │ word $f707
    ╰────
  help: Remove these upper bits
```

In most places, numbers have a limited size, often 8 or 16 bits. However, user-specified numbers are initially limited much higher, to 64-bit signed numbers, in order to allow computations that involve large intermediate values. When using those large numbers in these bit-limited places, the higher bits are simply cut off or truncated. This can be surprising depending on what exact instruction or macro the number is used in, so spcasm issues a specific warning by default.

## Errors

### spcasm::assign_to_macro_argument

```
```

Arguments to user-defined macros are specified with angle bracket syntax, like `<argument>`. When the macro is called, the arguments are replaced with the value given in the macro call. Therefore, it does not make sense to manually assign a specific value to the argument, like you can do with other kinds of labels. Instead, you can for example use local labels under the macro's special `\@` label.

### spcasm::audio_processing_error

```
  × failed to fill whole buffer
   ╭─[examples/errors/audio-processing.spcasm:1:1]
 1 │ brr "../binary.bin"
   · ─────────┬────────
   ·          ╰── While processing audio here
   ╰────
  help: This error is caused by the malformed input audio file "../binary.bin". If your audio player understands this file just fine, please file an spcasm bug.
```

The `brr` macro reads audio from WAV files and reports this error if the input file is not a valid WAV file. The exact error depends on what the third-party wav backend reports, but it is usually enough to check which file was included and test it with an audio player.

Known unsupported WAV features include all WAV extensions which compress audio, so use standard uncompressed WAV files instead.

### spcasm::empty_segment_stack

```
  × There is no segment on the stack
   ╭─[examples/errors/empty-segment-stack.spcasm:1:1]
 1 │ pullpc
   · ──┬──
   ·   ╰── Segment stack access here
   ╰────
  help: Macros like `pullpc` require that you push a segment to the stack beforehand with `pushpc`.
```

The segment stack stores segments for later reuse and is manipulated with `pushpc` and `pullpc` Similar to [missing_segment](#spcasmmissingsegment), you can't restore a segment if none has been saved yet.

### spcasm::file_not_found

```
  × File "examples/errors/this_definitely_does_not_exist" was not found
  ╰─▶ file not found. (os error 2)
   ╭─[examples/errors/file-not-found.spcasm:1:1]
 1 │ include "this_definitely_does_not_exist"
   · ───────────────────┬───────────────────
   ·                    ╰── File was requested here
   ╰────
```

_Technically_ this is a proxy error for all kinds of I/O errors, but it almost exclusively happens when spcasm can't find, open or read another file that was requested. This includes the main source file provided on the command line, but also other source or binary files. The operating system's original error is also contained in the report.

### spcasm::include_cycle

```
  × Include cycle detected while trying to include "examples/errors/circularly-included.spcasm.noerror"
   ╭─[examples/errors/circularly-included.spcasm.noerror:1:1]
 1 │ include "circular-include.spcasm"
   · ▲
   · ╰── This file's inclusion causes an include cycle
   ╰────
  help: The file examples/errors/circularly-included.spcasm.noerror was included:
        from examples/errors/circular-include.spcasm
        from examples/errors/circularly-included.spcasm.noerror
```

_(The example is slightly confusing; the pointer is not referring to the include macro at the beginning of the file, it's just a coincidence.)_

Assembly source file inclusion must be free of cycles, as including another file effectively copies that file's content in place of the `include` macro. This error is one of the few times when you'll see the include path of a file as it helps with troubleshooting. The file (a) that was attempted to be included is a file that directly or indirectly is a parent of the current file (b). That means that it (a) included the current file (b), which establishes a loop.

```
                 include cycle
[file a] <-----\ detected here
   |           |
includes       |
   |        includes
   v           |
[file b] --->--/
```

Note that it is totally fine to include a file multiple times within complex include graphs. In math lingo: The (directed) include graph does not have to be a tree, but it has to be acyclic.

### spcasm::invalid_addressing_mode

```
  × Invalid addressing mode `PSW` as first operand for `MOV`
   ╭─[examples/errors/addressing-mode.spcasm:1:1]
 1 │ MOV PSW,Y
   · ────┬───
   ·     ╰── For this instruction
   ╰────
  help: The instruction `MOV` accepts the modes A, X, Y, SP, (X), (X)+, $00, $00+X, $00+Y, $0000, $0000+X, $0000+Y, ($00+X), ($00)+Y here
```

This is one of the main instruction semantics errors together with [invalid_addressing_mode_combination](#spcasminvalidaddressingmodecombination). It generically means that the addressing mode in this position is not supported for this instruction. For some instructions, this error is reported even if the addressing mode would be legal paired with a different addressing mode or in a different position. This should be the domain of invalid_addressing_mode_combination, but that's not always the case.

To solve this error, it doesn't hurt to look at [the SPC700 reference](reference/README.md).

### spcasm::invalid_addressing_mode_combination

```
  × Invalid addressing mode combination: `SP` with `A` for `MOV`
   ╭─[examples/errors/addressing-combination.spcasm:1:1]
 1 │ MOV SP,A
   · ───┬───
   ·    ╰── Addressing mode invalid
   ╰────
```

This is one of the main instruction semantics errors together with [invalid_addressing_mode](#spcasminvalidaddressingmode). It specifies that while (some of) these addressing modes are allowed for this instruction, they're not allowed in this combination or order. In the example, both A as source and SP as target is possible with the MOV instruction, but when SP is the target, only X can be the source. Note that spcasm will not always report this error even if some of the addressing modes are legal for this instruction.

To solve this error, it doesn't hurt to look at [the SPC700 reference](reference/README.md).

### spcasm::invalid_arch_macro

```
  × Unknown architecture `65c816` specified
   ╭─[examples/errors/unsupported-arch.spcasm:1:1]
 1 │ arch 65c816
   · ─────┬─────
   ·      ╰── `arch` macro
   ╰────
  help: spcasm supports `arch` macros for compatibility with the Asar multi-architecture assembler. This macro specifies that the architecture of the assembly source is not (completely) SPC700, therefore spcasm cannot assemble this file.
```

See [valid_arch_macro](#spcasmvalidarchmacro); when compiling files originally targeted at Asar this error detects early when you are trying to compile non-SPC700 assembly.

### spcasm::invalid_constant

The given symbol constant (keyword) is invalid; this depends on the type of constant being dealt with. For example: A register was expected, but the symbol is not a register name: `A` and `X` are valid constants for registers, but `C` and `D` are not.

Note: This is a theoretical error the lexer can produce, but because invalid constants are assumed to be identifiers, this error does not currently surface to the user.

### spcasm::invalid_range

```
  × 4 is greater than 2
   ╭─[examples/errors/parser-invalid-range.spcasm:1:1]
 1 │ incbin "../binary.bin":4-2
   ·                       ─┬
   ·                        ╰── In this range
   ╰────
  help: Switch the range limits around: `2-4`
```

For range specifications, like when including binary files, the Asar style range syntax is a `start-end` format. Obviously, the start then needs to be before (or the same as) the end. Often you just accidentally swapped these limits.

### spcasm::references_in_macro_argument

```
  × Invalid use of references in an argument for `org`
   ╭─[examples/errors/label-in-org.spasm:3:1]
 3 │
 4 │ org dp_label
   · ─┬─
   ·  ╰── This macro
   ╰────
  help: Because the macro argument can determine a reference's position, resolving the argument value is not generally possible. For this reason, references are not allowed to be used in a macro argument.
```

Some macros, notably the `org` macro, disallow the use of references and other dynamic expressions in their argument(s) as that easily creates unresolvable circular dependencies. For example, consider this assembly code:

```asm
org loop
	nop
loop:
	nop
```

Let's ignore the org macro and assume that loop is at memory location 1. Then, however, loop must be at location 2 as the org macro now reads `org 1` (the nop instruction takes 1 byte). Then, however, loop must be at location 3 as the org macro now reads `org 3`. This line of reasoning will recurse to infinity and the above code is ill-formed.

spcasm doesn't yet have the capability of distinguishing between such a case and one where there is no problem at all, like:

```asm
custom_code_bank = $4000

org custom_code_bank
	nop
	; custom code...
```

Therefore, for now, spcasm disallows the use of references in these cases entirely.

### spcasm::macro_argument_outside_macro

```
```

Macro arguments are specific to user-defined macros and cannot be used in any way outside of them.

### spcasm::missing_global

```
  × There is no global label defined before the local label 'local_label'
   ╭─[examples/errors/missing-global.spcasm:1:1]
 1 │ .local_label:
   ·  ─────┬─────
   ·       ╰── Local label defined here
 2 │    nop
   ╰────
  help: Add a global label before defining this local label
```

Local labels always have a scope defined as the range between their "parent" global label and the next global label after them. The parent specifically is the global label directly before the local label. Therefore, any local label must be preceded by a global label that is its parent; a local label as the first label of a file is not allowed. An easy solution is to define a dummy label at the beginning of the file that is never used but provides an initial scope for local labels.

### spcasm::missing_operand

```
  × `MOV` takes at least one operand
   ╭─[<<input>>:2:1]
 2 │ loop:
 3 │   nop
 4 │   nop
 5 │   mov
   ·   ─┬
   ·    ╰── Takes at least one operand
 6 │   jmp loop
   ╰────
  help: Add an operand to this instruction
```

Similar to [operand_not_allowed](#spcasmoperandnotallowed). Some instructions instead require at least one operand.

### spcasm::missing_segment

```
  × There is no active segment here
   ╭─[examples/errors/missing-segment-after-pop.spcasm:1:1]
 1 │ pushpc
 2 │ db 0
   · ─┬─
   ·  ╰── This requires that there be a segment
   ╰────
  help: Start a new segment with `org <memory address>`
```

Segments specify where the currently-being-assembled binary data belongs in the address space. In a variety of situations, the assembler has no active segment, like after saving the current segment to the stack. You now need to provide a new segment, for example with an `org` macro or by restoring the saved segment from the stack with `pullpc`.

### spcasm::operand_not_allowed

```
  × `NOP` doesn't take any operands
   ╭─[examples/errors/no-operand.spcasm:1:1]
 1 │ nop a
   · ──┬─
   ·   ╰── Takes 0 operands
   ╰────
  help: Remove the operands of this instruction
```

Similar to [two_operands_not_allowed](#spcasmtwooperandsnotallowed). The instruction takes no operands, but one or two were specified.

### spcasm::range_out_of_bounds

```
  × The range 20000-50000 is out of bounds for the input file "../binary.bin"
   ╭─[examples/errors/range-out-of-bounds.spcasm:1:1]
 1 │ incbin "../binary.bin":20000-50000
   · ────────────────┬────────────────
   ·                 ╰── Out of bounds range defined here
   ╰────
  help: The input's length is 6
```

When providing a range for a binary file that is included, the range is allowed to exceed the end of the binary, at which point just everything until the binary's end is included. However, the start point must lie within the binary.

### spcasm::segment_mismatch

```
  × Segment at 0005 starts before the end of the previous one, which is 000a
   ╭─[examples/errors/segment-mismatch.spcasm:1:1]
 1 │  00 01 02 03 04 05 06 07 08 09
   ·                 ─┬
   ·                  ╰── Unexpected
   ╰────
```

When assembling different segments together, there must not be any overlap between their data. This is usually the result of starting a segment over in the wrong place; the `pushpc` and `pullpc` specifically allow you to come back to a segment after dealing with different segment(s) in between.

### spcasm::syntax::dangling_tokens

A syntax error issued when there are unconsumed tokens not used by the parser at the end of the file.

Note: This is a theoretical error forwarded from the parser backend. It is not known whether this error can actually be produced by spcasm's parser.

### spcasm::syntax::expected_token

```
  × Expected any of "\n", ".", "arch", "ascii", "asciiz", "brr", "byte", "db", "dd", "dl", "dw", "end", "identifier", "incbin", "include", "mnemonic", "org", "pullpc", "pushpc", "word"
   ╭─[errors/parser-dangling.spcasm:1:1]
 1 │ adc a,x
 2 │
 3 │ end
 4 │ 50
   · ─┬
   ·  ╰── This number is invalid here
   ╰────
```

The most common type of syntax error. "Token" in programming language lingo is the smallest sensible unit of text in the source code, which in this case was invalid. The parser expected a different token than what was actually found. A list of possible tokens (sometimes just one) is given by the parser, which hopefully helps you to discover the syntactical problem. In the above, for example, we can clearly see the parser expects, among other things, a macro, or a mnemonic, i.e. the start of an instruction.

### spcasm::syntax::invalid_bit_index

```
  × Invalid bit index `8`
   ╭─[examples/errors/parser-bit-index.spcasm:1:1]
 1 │ set1 33.8
   ·         ┬
   ·         ╰── Bit index is invalid
   ╰────
  help: Use a bit index between 0 and 7 inclusive
```

Some addressing modes contain an intra-byte bit index; this has to be between 0 and 7 inclusive as there are 8 bits to a byte.

### spcasm::syntax::invalid_number

```
  × Invalid number: invalid digit found in string
   ╭─[examples/errors/parser-invalid-number.spcasm:1:1]
 1 │ label = %34af
   ·         ──┬─
   ·           ╰── invalid digit found in string
   ╰────
```

```
  × Invalid number: number too large to fit in target type
   ╭─[examples/errors/parser-invalid-number.spcasm:1:1]
 1 │ label = $34affffffffffffffffffffffff
   ·         ─────────────┬─────────────
   ·                      ╰── number too large to fit in target type
   ╰────
```

Numbers need to conform to a specific format that also depends on the radix, e.g. whether you're specifying numbers in hexadecimal or binary. The exact error, two different examples shown above, gives you a good clue of what is wrong with this number.

### spcasm::syntax::invalid_test_comment

```
  × Test comment has invalid format
   ╭─[examples/errors/parser-test-comment.spcasm:1:1]
 1 │ nop ;= ajsdkfl
   ·     ────┬───
   ·         ╰── This ';=' comment is invalid: invalid digit found in string
   ╰────
  help: Test comments consist of a series of space-delimited bytes, given as hexadecimal, for example `;= 0F AA B8` for three bytes
```

Test comments are only used in spcasm's testing system, so you won't see this error unless you're developing spcasm and writing tests. The specific test comment format is a sequence of space-delimited bytes given in hexadecimal.

### spcasm::syntax::missing_token

```
  × Expected any of "\n", ".", "arch", "ascii", "asciiz", "brr", "byte", "db", "dd", "dl", "dw", "end", "identifier", "incbin", "include", "mnemonic", "org", "pullpc", "pushpc", "word"
   ╭─[examples/errors/parser-missing-token.spcasm:1:1]
 1 │ label:
   ╰────
```

A similar error to [expected_token](#spcasmsyntaxexpectedtoken), though in this case the file ends without some token that needs to occur in order for the file to be syntactically valid. The error message is intentionally similar to expected_token as there mostly is no difference from the user's perspective.

### spcasm::syntax::unexpected_character

```
  × Unexpected character ö
   ╭─[examples/errors/parser-unexpected-char.spcasm:1:1]
 1 │ ö
   · ▲
   · ╰── Unexpected
   ╰────
```

A generic early error when spcasm can't at all make sense of some character in the assembly source.

### spcasm::two_operands_not_allowed

```
  × Two operands are not allowed for `POP`
   ╭─[examples/errors/two-operands.spcasm:1:1]
 1 │ pop x,a
   · ───┬──
   ·    ╰── Only takes 1 operand
   ╰────
  help: Remove the second operand
```

Similar to [operand_not_allowed](#spcasmoperandnotallowed). The instruction only takes one operand/addressing mode, but two were given.

### spcasm::unresolved_reference

```
  × reference 'no_exist' can not be resolved to a value
   ╭─[examples/errors/unresolved-label.spcasm:1:1]
 1 │ mov a,no_exist
   · ──────┬───┬───
   ·       │   ╰── 'no_exist' defined here
   ·       ╰── Used here
   ╰────
  help: Any symbolic reference must be defined somewhere. Did you misspell the reference's name?
```

Evidently, references which are not defined anywhere in the assembly source cannot be used in instructions or macros. Defining a reference involves either using it as the label for some instruction (then the memory address will be filled in automatically), or assigning it a literal value to make it act like a constant.

```assembly
label: nop ; reference has the memory address of the nop instruction
label = 50 ; reference has value 50
```

Often, however, a reference is simply misspelled.
