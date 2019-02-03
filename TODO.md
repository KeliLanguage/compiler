[x] implement intellisense (refer Haskero)
    - the intellisense should show every possible function for a type
    - For example, if I type `1.`, a list of functions that takes `Int` as any parameter should show up


[] each function decl should be assign a uniq UID, to ease transpilation

[] error reporting does not contain accurate location

[x] define factorial function

[x] js ffi

[x] problem: cannot map JS boolean to Keli boolean

[] should check for duplciated IDS first before going into analyzing

[] import base code when starting repl

[x] when using tag constructor, should be prefixed by the tagged union name, e.g. use `list.cons` instead of `cons`

[x] replace ":" with ".as" for type-annotated expressions

[x] remove singleton constant feature (can be emulated using tagged union)

[x] change primitive type to PascalCase

[] implement generic tagged union

[] incomplete function expr does not capture function call chaining

[] combine syntax highlighter with language server

[] change keyword `carry` to `load`

[] location of error regarding (expected expr not type) is inaccurate