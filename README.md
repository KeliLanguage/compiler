# compiler-demo

## Namings
Refer Glossary.md

## How to setup this project?

```
stack build
```

## How to run test?

```
stack test --file-watch
```

## How to run ghci?

The following command is to prevent stack overflow due to unknown infinite loop.

Refer https://stackoverflow.com/questions/35342591/haskell-limit-ghci-memory

```sh
stack ghci --package pretty-simple --ghci-options="+RTS -M256m -K256m -RTS -interactive-print=Text.Pretty.Simple.pPrint" 
```

## Versioning

We will be using SemVer + CalVer, as the following format:

```
MAJOR.YY.MM.DD
```