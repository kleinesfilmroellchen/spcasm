# Architectural Overview

spcasm processes assembly code in several steps, which can be thought of as roughly equivalent to "passes" in traditional assemblers. The processing can be split up into four main steps, parsing, (early) semantic lowering, symbolic assembly and output.

In terms of performance, parsing and label resolution have consistently been the most expensive operations, each taking at least 20% of run time. No exact metrics on memory usage exist, since spcasm is a highly memory-efficient program that consumes only a few megabytes peak.

## Parsing

A custom **lexer** with minimal lookahead splits the input text into tokens and already performs some degree of high-level processing, like parsing numbers and test comments (if applicable) as well as distinguishing between directives, mnemonics and references.

Next, the **token stream is transformed** and some new tokens are created that the simple lexer cannot deal with. This step is necessary as the SPC700 assembly language with complex math expressions is not LR(1) (in fact, as far as I can tell it's context-sensitive), so we cannot pass the plain tokens to the parser. Instead, some transformations with significant lookahead need to happen, for example determining what parenthesis are used for (indexing as an addressing mode or math expressions?), combining "+X" and "+Y" and appending a newline to enable the top-level producer grammar.

Finally, the token stream passes to an **LR(1) parser**. The parser is an auto-generated [LALRPOP](https://github.com/lalrpop/lalrpop) parser that receives the more context-aware modified token stream. The .lalrpop file additionally contains driver code that builds the AST and performs various early decisions and resolutions, like selecting direct page addressing if possible.

## Early semantic lowering

**Source inclusion** is handled at this step. The `include` directives include the assembly source code from other files, and after the AST is finalized, all requested additional files are resolved. The assembler keeps a memory cache of already-parsed files, so that if you have commonly-included files, the cost of including them more than once is reduced. Handling other files goes through the exact same pipeline as the main file, so it repeats steps 1-5 and possibly 6 if there are included files in the included file. Circular includes are not possible for obvious reasons (though non-circular repeated includes are allowed as there are many valid use cases) and will be detected even before the lexer runs. After an included file's AST is finalized, it is copied into the original AST where it replaces the `include` directive.

In **early directive resolution**, the assembler detects all directives in need of expansion. At the time of writing, these are user-defined macros, conditionals, and repeat directives. They are then resolved by replacing them with generated AST. In the case of user-defined macros, resolution must succeed, since later stages of the assembler cannot handle them. For other directives, resolution might fail, since their conditionals or repeat counts may contain unresolved references. This is acceptable as there is a secondary late resolution step. spcasm's macro functionality is far more sophisticated than simple copy-paste, it can rather be thought of as a function inliner in a traditional compiler.

Next, **label linking** is performed. There are several kinds of non-global references who need to be linked to their relatives later than during parsing, such as relative labels, local labels, and pseudo-labels. This is fixed in several independent analysis steps, and afterwards, only proper labels that have pointers to their definition should exist in a well-formed program.

## Symbolic assembly

Symbolic assembly is initiated with **program segmentation.** The assembler splits the program into its physical segments by linearly traversing the fully expanded AST and handling segment control macros, such as `org` or `pushpc`. The result is a collection of segments with known starting addresses and the contained program elements, still in AST form.

**Direct page reference optimization** comes next. Given the new knowledge of segments, the assembler can infer memory addresses for most kinds of program elements. Therefore, the assembler is able to deduce which references will live in the direct page. This is important as many instructions are smaller and faster when using direct page addressing, so they should be used whenever possible. The assembler uses an iterative optimization process that keeps as many labels as possible in the direct page. For a detailed explanation, see the well-commented `spcasm::parser::AssemblyFile::optimize_direct_page_labels` function. The optimization problem solved here has been compared to linker relaxation for jump offsets and thunks; however while optimal linker relaxation is most likely NP-hard, spcasm (minus some bugs) solves the direct page reference optimization problem, which is in P, optimally.

Now, the assembler assembles a mix of high and low-level memory data from instructions and directives. Where possible, concrete memory data is provided, but in other cases, symbolic data like "lowest byte of this reference's address" is used instead when necessary. It is important to notice that memory addresses are fixed after this step.

**Late directive resolution** is a part of symbolic assembly. Some directives need to be resolved as soon as labels can have fixed positions, which is after reference optimization. This is because they contain other blocks of instructions and directives, and the entire structure needs to be flattened into a static list of elements that can be assembled without ambiguity of sizes. If any directives were not resolved in the early resolver, they are resolved here in the late resolver, and (for now) we run a reference resolution pass before each directive to make sure that as many references as possible are available for use in the directive. While this doesn’t permit every label to be used in complex directives like `if` and `repeat`, it does allow them to be a lot more powerful than would be possible otherwise, without complicating the assembler architecture.

## Code generation

Because addresses were fixed in the last step, all remaining symbolic data can now be resolved to concrete data. This is done in several passes to allow e.g. mention of references before they are defined. The step both involves obtaining concrete values for symbols based on the fixed memory layout, and emplacing concrete data based on symbols that were newly resolved. For example, a reference's value can be resolved to the memory location of the data that is tagged to be the position of the reference. Then, references to the reference can be replaced by the actual memory address.

Finally, the binary data is converted into the **output format**. Before this step, the binary data exists in several independent segments. Now, depending on the output format, the data is assembled into its final form and written out. The details of this step of course depend on the format, such that a format like ELF will keep the segments separate while a ROM image will concatenate segments.

## Intermediate representations

The assembler employs a few intermediate representations that are roughly delineated by the major steps of assembly:

- A list of tokens is produced by the two-step lexer and transformed into an **abstract syntax tree** (AST) by the parser.
- The AST is progressively simplified into a (near-) linear format by the end of semantic lowering.
- Symbolic assembly transforms the AST into a **segment list**, which starts out abstractly as a list of AST nodes (program elements), which can still contain nesting. The segment list contains a separate list of elements per program segment, where program segments have fixed starting addresses.
- By the end of symbolic assembly, the segment list now consists of **symbolic memory values**, which may contain references and various calculations. The main semantic difference to the previous step is that all of these memory values only occupy one byte, thereby all sizes are known and final reference resolution can occur.
- After final reference resolution, all symbolic values can be collapsed to bytes, and the resulting **segment list of binary data** is suitable for conversion into all output formats. Each output format uses its own in-memory representation of the output file, such as the ELF datastructures of `object` for ELF output.
