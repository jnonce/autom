# Automata

Pattern matching logic application in Haskell.  Similar to Parsec based
combinators, simplified for educational purpose.

The primary types in the system are `MachineT` type and `Stream` typeclass.
`Stream` is a list-like structure which can provide tokens one by one.
`MachineT` defines a pattern matching machine which holds a stream and can
extract tokens and match.

A sample:

```Haskell

word :: Machine Text Text
word = (T.pack <$> many alphaNum) <?> "word"
```
