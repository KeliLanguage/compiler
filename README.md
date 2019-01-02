# compiler-demo

## How to setup this project?

```
stack build
```

## How to run test?

```
stack test
```

## How to run ghci?

The following command is to prevent stack overflow due to unknown infinite loop.

Refer https://stackoverflow.com/questions/35342591/haskell-limit-ghci-memory

```sh
stack ghci --package pretty-simple --ghci-options="+RTS -M256m -K256m -RTS -interactive-print=Text.Pretty.Simple.pPrint" 
```
