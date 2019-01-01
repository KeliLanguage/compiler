# compiler-demo

## How to run ghci?

The following command is to prevent stack overflow due to unknown infinite loop.

Refer https://stackoverflow.com/questions/35342591/haskell-limit-ghci-memory

```sh
stack ghci --ghci-options="+RTS -M256m -K256m -RTS"
```
