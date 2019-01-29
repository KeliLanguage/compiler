[] implement intellisense (refer Haskero)
    - the intellisense should show every possible function for a type
    - For example, if I type `1.`, a list of functions that takes `int` as any parameter should show up


- each function decl should be assign a uniq UID, to ease transpilation

- error reporting does not contain accurate location

[x] define factorial function

[x] js ffi

[x] problem: cannot map JS boolean to Keli boolean

[] should check for duplciated IDS first before going into analyzing

[] import base code when starting repl

[] when using tag constructor, should be prefixed by the tagged union name, e.g. use `list.cons` instead of `cons`