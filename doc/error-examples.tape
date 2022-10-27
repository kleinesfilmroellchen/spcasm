Output doc/error-examples.webm
Output doc/error-examples.gif

Set Framerate 20
Set FontSize 26
# Set FontFamily "CascadiaCode"
Set Width 1200
Set Height 2000

Hide
Type "export PATH=$PATH:target/debug/"
Enter
Type "clear"
Enter
Show

Type "spcasm examples/test.spcasm"

Sleep 500ms
Enter
Sleep 5s

Hide
Type "clear"
Enter
Show

Type "spcasm examples/errors/circular-include.spcasm"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm examples/errors/range-out-of-bounds.spcasm"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm examples/errors/parser-bit-index.spcasm"

Sleep 500ms
Enter
Sleep 2s

Type "spcasm examples/errors/unresolved-label.spcasm"

Sleep 500ms
Enter
Sleep 6s
