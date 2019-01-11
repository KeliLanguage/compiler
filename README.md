# Keli Compiler

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
stack ghci --package pretty-simple --ghci-options="+RTS -M256m -K256m -RTS -interactive-print=Text.Pretty.Simple.pPrint -Wall" 
```

## Versioning

We will be using SemVer + CalVer, as the following format:

```md
MAJOR.YY.MM.DD
```

## Some extra documentation about Data.Map.Ordered

```hs
-- assocs means valuesOf
-- For example,
--  assocs x
--      means, get the list of key-value pair from x (which is arranged according to insertion order)
-- |> means insert
-- For example,
--      x |> (key, value)
--          means, insert (key,value) into x
-- For more please refer http://hackage.haskell.org/package/ordered-containers-0.1.1/docs/Data-Map-Ordered.html
```