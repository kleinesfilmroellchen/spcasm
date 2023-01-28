# Directives

* auto-gen TOC;
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

## `incbin`

This directive allows to include a binary file verbatim at the current location. The three supported syntaxes are:

```asm
incbin "file"                   ; entire file
incbin "file", start [, length] ; vasm-style
incbin "file": start [- end]    ; Asar-style
```

Vasm-style includes can specify a number of bytes to include in addition to the start byte. Asar-style includes can specify the end index of the include; this index is exclusive. If length or end are not specified, both variants include from the given start to the

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
- `nodirectory`: This is a forward-compatibility option that currently does nothing. In the future it will exclude this BRR sample from auto-generated sample directories.

## `end`

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
