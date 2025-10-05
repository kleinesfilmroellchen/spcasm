# Directives

- auto-gen TOC;
  {:toc}

Directives, also [sometimes called "macros"](../terminology.md#macro) in other assemblers, direct the assembler to perform some sort of action that is not "assemble this instruction". Most directives are self-contained, some however create certain kinds of blocks.

## `include`

This directive will include and assemble the contents of another assembly source file at the current location.

```asm
include "library.s"
```

The path is always relative to the location of the source file that the directive is called within. Some things to note about including source files and working with many-file projects:

- The include procedure is not entirely a dumb include. The other file must be at least parsable on its own. You cannot, for example, create partial macros where two files have only the beginning or the end of a macro definition.
- Any file may be included multiple times, but circular includes are forbidden. For example, it is very useful to include a list of variable location definitions multiple times in multiple source files.

## Table directives

Table directives include any directive that contains a list of values to place into the assembled output file verbatim. The general syntax is always

```asm
directive number_1, number_2, number_3 [, ...]
```

where `directive` is a specific table directive (see below) and `number_1`, `number_2`, ... are values, possibly expressions, that should be placed into the output in that order. The values are separated by commas, and there may be a trailing comma. There may not be any values, in that case this directive does nothing.

The supported table types are:

- `db`/`byte`: 8-bit entries.
- `dw`/`word`: 16-bit little endian entries.

## `ascii`, `asciiz`

Similar to the table directives, these two directives allow including verbatim ASCII text into the output file at the current location. Note that spcasm does not check the contained string in any form, so e.g. characters above 127 are possible via escapes.

```asm
ascii "Hello, world!"
asciiz "Hello,\xffbinary world!"
```

While `ascii` includes nothing but the provided characters, `asciiz` additionally includes a null terminator after the last character. This is useful if you are dealing with strings in a C-like way, using the null terminator to detect their end.

## Fill directives

> Note: The invocation and behavior of fill directives is rather strange, and this way of explaining how they work can rarely be found elsewhere. Keep in mind that fill directives only exist in this form for Asar compatibility, and that I would have designed their syntax very differently.

Fill directives repeat a fill value across a potentially large range of memory. There are three available fill directives, which use the keywords `fill`, `fill align`, and `pad`.

```asm
; Repeat the value to fill exactly 15 bytes
fill 15
; Repeat the value until an alignment of 8 is reached (address divisible by 8)
fill align 8
; Fill to alignment of 4, then fill another 5 bytes
fill align 4 offset 5
; Fill up to address $8086
pad $8086
```

`fill align` is the only variant that takes an extra parameter, which specifies additional fill to be added after the alignment is reached. In spcasm, `fill align 1 offset xyz` and `fill align 0 offset xyz` are equivalent to `fill xyz`.

Note that the fill value can have a size of 1 to 4 bytes. If this does not fit the amount of filled bytes exactly, the value will be truncated. As values are output in little endian as usual, that means that upper byte(s) are truncated, like with instructions.

The value that is used for filling memory, as well as its size, is determined by separate directives. `fill` and `fill align` use one set, while `pad` uses another. A fill value directive is effective from the moment it is used in source code on; segments have no effect on this.

```asm
; For fill and fill align
fillbyte $12
fillword $1234
filllong $123456
filldword $12345678
; For pad
padbyte $12
padword $1234
padlong $123456
paddword $12345678
```

## `incbin`

This directive allows to include a binary file verbatim at the current location. The three supported syntaxes are:

```asm
incbin "file"                   ; entire file
incbin "file", start [, length] ; vasm-style
incbin "file": start [- end]    ; Asar-style
```

Vasm-style includes can specify a number of bytes to include in addition to the start byte. Asar-style includes can specify the end index of the include; this index is exclusive. If length or end are not specified, both variants include from the given start to the end of the file.

File paths are relative to the current file, as with `include`.

## `brr`

The `brr` directive allows to generate and include BRR data from audio files. If you already have compiled BRR data, use the `incbin` directive instead.

```asm
brr "my_sound.wav"
```

As with `incbin`, the data from the file-relative include path is placed at the current location. The only supported file format is standard uncompressed WAV. You cannot currently change which part of the file is used; the entire file will always be compressed and included. spcasm's BRR encoder will always run at maximum optimization settings, brute-forcing all possible settings for all BRR blocks. For BRR data fitting within the 64KB address space, this is not a problem, as the BRR encoder can encode 64KB worth of BRR data in less than 200ms on a release build of spcasm.

The `brr` directive accepts several optional extra parameters:

```asm
brr "filename" [range] [options...]
```

The range syntax is identical to the syntax used by the [`incbin`](#incbin) directive. It specifies the range of samples that should be compressed to BRR (not the range of final BRR data!). The options are a list of identifiers that change the behavior of the BRR processing in various ways. Currently, these are supported:

- `autotrim`: Turn on automatic sample trimming. Many samples contain "DC silence", i.e. a constant sample value over a long period of time, at the beginning and/or end of a file. DC silence is often a side effect of imprecise trimming of audio clips and has no effect on the sound, as it is per definition silent. However, DC silence takes up valuable storage space and should therefore be removed wherever possible. Therefore, to allow you to be a bit more sloppy when trimming the input files, spcasm can detect pure DC silence at the beginning and end of a sample and remove it. Note that this removal of unnecessary silence will happen after the clip was trimmed with the range specification, so you can use the two features in combination to first select the relevant sample in a longer file, and then trim silence from it with `autotrim`.
- `nodirectory`: Exclude this BRR sample from the automatically generated sample table.

### `sampletable`

The `sampletable` directive auto-generates the hardware sample table (or "directory") which the DSP needs to find BRR samples in memory. The details of the table's layout and how to set its location are beyond the scope of this documentation, but suffice it to say that you still need to point the DSP at your table via the `DIR` hardware register. The table is automatically aligned to a page boundary if necessary, because the hardware requires this.

All BRR samples without the `nodirectory` option are automatically part of this table. You can use this directive multiple times to create multiple copies of the identical table. The BRR samples are added in the order they appear in the assembly code, and in the future there will also be a way of obtaining their position at compile time.

The `sampletable` directive accepts several optional extra parameters:

```asm
sampletable [options...]
```

The options are a list of identifiers like with `brr`. Currently, these options are supported:

- `noalign`: Prevent automatic alignment of the sample table to a page boundary. This is required for hardware, so spcasm does it by default, but if you want to be very careful about your memory usage, you can disable it. spcasm will still check your table's position and might issue an error.

In general, `sampletable` provides sane and easy defaults for quickly generating a sample table. If you have more specific needs, like overlapping samples, specific sample orderings, overlong or misaligned tables, you should manually specify the sample table(s) instead.

## `endasm`

This directive takes no arguments and will stop the assembler as soon as possible. Due to how segments work in spcasm, this directive might not immediately stop assembly for the entire binary, but it will definitely stop assembly within its segment. This is an old feature inherited from more primitive assemblers and its use is discouraged.

## Segment control directives

spcasm assembles your binary in several segments. These are much more important on banked architectures such as the main SNES processor, but it still allows for more flexibility of putting code and data into specific regions of memory. Each consecutive unit of data is called a "segment", and each segment has a fixed starting address. During assembly, you can switch between segments, start new segments wherever you want, and even retain a stack of segment which may be reactivated. However, at the end, individual segments must not overlap or spcasm will throw an error.

At the beginning of the file, there is no active segment, so you initially have to start one with `org`.

### `org`

The `org` directive is the most important and fundamental segment control directive. `org` starts a new segment at the specified address, ending any previous segment:

```asm
; Normal code start location
org $200

org $4000+30
```

Because segment start locations must be known early on, using references in the value is quite restricted.

### `startpos`

The `startpos` directive specifies that the program’s entry point should be at the current position (that is, at the next instruction). The entry point is what the program executes after being loaded by the ROM loader.

Note that this directive is not always required depending on the output format. If you are assembling a raw RAM image, the entry point will not stored anywhere anyways.

If you implement a custom fastloader, the code at the entry point is what should be uploaded with the bootrom uploader.

```asm
org $1000
startpos
  ; your entry point code, e.g. initialization...
main_function:
  mov a, #2
```

### Segment stack

The segment stack can be controlled with the `pushpc` and `pullpc` instructions. `pushpc` pushes the current segment to the segment stack, and there will not be an active segment afterwards. `pullpc` pulls the last segment from the segment stack, reactivating it and allowing you to continue appending data to its end. This is currently the only way of continuing to put data into an already-existing segment that was "interrupted", so to speak.

```asm
org $200
; Imagine this file containing the main code loop and some main routines.
include "main_function.s"
; For readability, we interrupt this segment...
pushpc
; ... to continue at a high address that is used for BRR samples.
org $9000

brr "sample1.wav"
brr "sample2.wav"
brr "sample3.wav"

; Now we continue with our code!
pullpc
include "sfx_engine.s"
```

```asm
{{#include complex_segments.s}}
```

## User-defined macros

Sometimes just referred to as "macros", user-defined macros define reusable code snippets that can be repeatedly placed anywhere in the code. Often, they function like a subroutine without the overhead of calling or passing arguments. Macros are started with the `macro` directive and end with the `endmacro` directive. The latter never takes arguments, but for the former, the possible argument formats are possible:

```asm
macro my_macro
endmacro

; Asar-style
macro my_macro_with_arguments(first_argument, second_argument [, ...])
endmacro

; Vasm-style
macro my_other_macro_with_args, first_argument, second_argument [, ...]
endmacro
```

Both styles of argument lists are equivalent and can contain a trailing comma; but Asar-style macro argument lists must always contain the closing parenthesis.

The named arguments form references which are only and uniquely defined within the macro's body. You can access them with angle brackets.

```asm
macro load_a_with(value,)
  mov a, # <value>
endmacro

macro load_a_and_x_with(value_a, value_x)
  mov a, # <value_a>
  mov x, # <value_x>
endmacro
```

Macros are called by prefixing the macro's name with a percent sign. The actual parameter list must match the formal argument list from the definition exactly, but any values can be used. There are no limitations on the order of macro definition and usage, though macros cannot be defined within other macros.

```asm
; Given the above definitions, this assembles to exactly three instructions.
some_label:
  %my_macro()
  %load_a_with(70)
  %load_a_and_x_with($50-10, some_label)
```

While macros might call other macros, there must not be any infinite recursion and spcasm has a configurable limit for this.

Within a macro, you can use the label \@ as a unique global label which will not collide with any other global label anywhere else, not even in another call of the same macro. This allows you to create a unique global label scope for each macro call and use local labels with the same name within it.

## Conditional compilation

Conditional compilation allows spcasm to decide at compile time which code to assemble into the output or not depending on user-defined conditions. The simplest form of this is the `if` construct, which behaves like you expect from other programming languages:

```asm

mode = 3 ; or 4, 5, 6, ...

if mode == 4
  jmp mode_4_handler
elseif mode == 5
  jmp mode_5_handler
else
  jmp other_modes_handler
endif
```

The value in the same line as the `if` specifies the condition that decides which branch is taken. spcasm has no special boolean type. As is common in programming languages, any non-zero value is considered true, and zero itself is considered false.

After an `if` there must be an `endif` or `else`. As you can see in the example, `elseif` can be chained arbitrarily.

Note that depending on where you use conditional compilation, spcasm might have different requirements for when the condition's value needs to be known. In basically all cases, the condition needs to be resolved before assembly starts.

## `repeat`

Similar to conditional compilation, the `repeat` directive can be used to repeat an assembler block any number of times, including not at all. The syntax is straightforward like the `if` directive:

```asm
delay_200:
; delay for 200 cycles
repeat 200
  nop
endrepeat
```

The number after `repeat` repeats the block however often specified. The block is terminated by `endrepeat` This can be any kind of calculation, as usual, as long as it can be resolved before code generation needs to happen. (Therefore, many labels will not work.)

A repetition count of zero (or negative) is allowed and will not repeat the block at all, thereby removing it.

Sometimes it is useful to know inside the block on which iteration of the repeat it’s currently on. For this, the special `repeatcount` variable is reserved that gets replaced with a normal zero-based index of the iteration. For example:

```asm
increasing_number_table:
repeat 100
  db repeatcount
endrepeat
```

This produces a byte table of all the numbers from 0 to 99 inclusive.

Note that only the innermost repeat’s `repeatcount` is accessible, just like with variable shadowing. A local reference can be assigned to the `repeatcount` if you need access to multiple `repeatcount` values in nested repeat blocks.
