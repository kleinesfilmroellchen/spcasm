Output doc/error-examples.webm
Output doc/error-examples.gif

Set Framerate 20
Set FontSize 26
# Set FontFamily "CascadiaCode"
Set Width 1200
Set Height 2000

Hide
Type "export PATH=${PATH}:target/debug/"
Enter
Type "clear"
Enter
Show

Type "spcasm tests/opcodes.s"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm tests/errors/circular-include.spcasmtest"

Sleep 500ms
Enter
Sleep 5s

Hide
Type "clear"
Enter
Show

Type "spcasm tests/errors/range-out-of-bounds.spcasmtest"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm tests/errors/parser-bit-index.spcasmtest"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm tests/errors/unresolved-label.spcasmtest"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm tests/errors/addressing-combination.spcasmtest"

Sleep 500ms
Enter
Sleep 6s
