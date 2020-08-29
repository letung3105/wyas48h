# Write your first parser (Chapter 01 - Write You a Scheme in 48H)

+ Glasgow Haskell Compiler (GHC)
+ Parsec (comes with GHC)


## Parser

+ Instance of a monad, contains extra information about the data that is being parsed.
+ `>>` binding is different with every monad with `Parsec`: the parser parses, return its result and passes the remaining string to the next parser.


## Functor

+ `<$>` takes a function `(a -> b)` and apply it to the data encapsulated in a type.


## Monad

+ `>>`, `>>=`, and `do` behaves differently with different module.
	+ `>>` is generally for actions that do not return a value.
	+ `>>=` is generally for immediately pass the value to the next action.
	+ `do` if otherwise.
