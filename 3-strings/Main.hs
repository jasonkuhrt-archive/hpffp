module Chapter3 where
{-

## 3.1 Printing strings

* "string"
* data structure used to contain text
* most computer langauges have this type/terminology
* usually represented as sequences (AKA lists) of characters
* in Haskell, a list of characters



## 3.2 A First Look at Types

* "types"
* a way of categorizing values
* e.g. integers, fractions, booleans, Char, String

* ghci `:type` will return type info for an expression

* "type signature"
* line of code that defines the types for a function or expresion

* single quote is character
* like C, .Net

* double quote is string
* like Clojure, JavaScript (though there single works too)

* "type alias" (AKA type synonym)
* a type name that has a different name underneath
* usually used for convenience
* String is another name for [Char]



## 3.3 Printing simple strings

* functions to send strings to stdout include
  print, putStrLn, putStr
* `print` uses the Show implementation of types to convert values before printing, whils putStr et al only work with strings.
* as a result the result of `print` for strings are quoted
-}

print1 :: IO ()
print1 = putStrLn "hello world!"

{-
* main is the default action of a built executable, or REPL run
* it is not a function but is often a series of instructions to execute e.g. applying functions for side-effects
* build tool `stack` requires a Main.hs file with `main` defined
* other support modules in a project do not have to have a `main`
* IO type stands for input/output
* IO is special in Haskell
* denotes a type that in addition to being a function or expression it also involves effects
* effect example: printing to screen
* a .ghci file in ~ can be used to place ghci commands that you want run at boot every time
* `do` syntax is sugar for sequencing actions

* "concatenate"
* to link together
* in programming usually refers to joining linear sequences

* types should usually be specified for top-level types (note to self: anyways linter defaults are to enforce this)
-}
