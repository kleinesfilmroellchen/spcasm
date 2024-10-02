# spcfile

A library for reading and writing .spc files, used for storing SNES game audio.

This library is part of [`spcasm`](https://github.com/kleinesfilmroellchen/spcasm) and follows spcasm's release schedule and version numbers.

## Examples

Use [`parser::parse_from_bytes`] to parse an SPC file from a byte slice. It either returns a [`nom`] parsing error, or an [`SpcFile`] struct. This struct is a straightforward representation of the SPC file format structure, split into [`SpcHeader`] and [`SpcMemory`].

The `spcdump` example is a simple utility to print out the parsed result of any .spc file.

## A Word of Warning about .spc

.spc, despite being fairly simple (and not having to convey much information), is an insanely bad file format. Here are some of the issues relating to spcfile's reading behavior:

- Header metadata can take one of two formats (binary or text), and while modern emulators use emulator IDs above 0x30 to indicate text format, this may fail with files that have been written by older or partially non-compliant emulators. Additionally, some specific combination of emulator flags and other tags may lead this detection to fail. Luckily, text vs. binary only affects a few metadata fields.
- Text encoding is not specified anywhere, and I fear (and [spctag corroborates this](https://github.com/ullenius/spctag?tab=readme-ov-file#character-encodings)) that many of the old writers used whatever Windows code page the system happened to be on. spcfile assumes UTF-8, but will do a lossy conversion that drops any unrecognizable characters.

Additionally, spcplay currently has no support for extended ID666 metadata at the end of the file.
