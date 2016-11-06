module Chapter3 where

{-# ANN module "HLint: ignore Use String" #-}

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



## 3.4 Top-Level Versus Local Definitions

* top-level declaration
* visible in entire module
* not child to any other expression

* "local declaration"
* child to another expression
* not available outside parent expression

### Intermission Exercises: Scope

1. REPL: is y in scope for x?

  let x = 5
  let y = 7
  let z = x * y

  YES

2. REPL: is h in scope for function y

  let f = 3
  let g = 6 * f + h

  NO!

3. Source: can `area` execute?

  area d = pi * (r * r)
  r = d / 2

  NO! `d` is not in scope for `r` making it invalid which propogates to `area`. `pi` is ok because its from prelude

4. Source: now?

  area d =
    pi * (r * r)
    where
    r = d / 2

  YES



## 3.5 Types of Concatenation Functions

* anytime we want to refer to an operator (AKA infix function) not in infix position we must syntactically wrap it in parentheses, even in ghci e.g. `:type (+)`

* concat is like flatten from JavaScript lodash

  concat [[1],[2],[3]] == [1,2,3]

* GHC 7.10 introduced the Foldable typeclass making certain functions like `concat` more generic. e.g.

  concat (Just [1,2,3]) == [1,2,3]

* in type syntax, a lower-case alpha string, is a type variable AKA polymorphic
* for example `concat` has type Foldable f => f [a] -> [a] where is a type variable. `concat` does not care what type `a` will be and thus can work across different types e.g.

  concat (Just [1,2,3]) == [1,2,3]
  concat (Just "foobar") == "foobar"

### Intermission Exercises: Syntax Errors

Will these compile and if not fix them.

1. ++ [1,2,3] [4,5,6]
   (++) [1,2,3] [4,5,6]

2. '<3' ++ ' Haskell'
   "<3" ++ " Haskell"

3. concat ["<3", " Haskell"]
   OK!



## 3.6 Concatenation and Scoping

* `++` is right associative



## 3.7 More List Functions

*  since string type is just an alias type for a list of characters all functions that can act on lists can act on strings



## 3.8 Chapter Exercises

### Reading Syntax

Fix syntax errors as needed

1a. concat [[1,2,3]], [4,5,6]
    concat [[1,2,3], [4,5,6]]
1b. ++ [1,2,3] [4,5,6]
    (++) [1,2,3] [4,5,6]
1c. (++) "hello" " world"
    OK!
1d. ["hello" ++ " world]
    ["hello" ++ " world"]
1e. 4 !! "hello"
    "hello" !! 4
1f. (!!) "hello" 4
    OK!
1g. take "4 lovely"
    take 4 "lovely"
1h. take 3 "awesome"
    OK!

2. Match set of inputs to set out of outputs

2a. concat [[1*6], [2*6], [3*6]] == [6,12,18] -- d
2b. "rain" ++ drop 2 "elbow" == "rainbow" -- c
2c. 10 * head [1,2,3] == 10 -- e
2d. (take 3 "Julie") ++ (tail "yes") == "Jules"
2e. concat [tail [1,2,3], tail [4,5,6], tail [7,8,9]] == [2,3,5,6,8,9] -- b

### Building Functions

1a. "curry is awesome" ++ "!"
1b. (take 1 . drop 4 $ "curry is awesome!") == "y"
1c. drop 9 "Curry is awesome!" == "awesome!"

2. Convert the above into functions
-}

exclaim :: String -> String
exclaim = (++ "!")

slice5th :: [a] -> [a]
slice5th = take 1 . drop 4

drop9 :: [a] -> [a]
drop9 = drop 9

{- 3. create a thirdLetter function -}

thirdLetter :: [Char] -> Char
thirdLetter = (!! 2)

{- 4. Create a function that given an index returns the letter for some constant string (you choose)-}

letterIndex :: Int -> Char
letterIndex = (string !!)
  where
  string = "Roman"

{- 5. Use drop/take to transform "Curry is awesome" to "awesome is Curry. Call the function rvrs. "-}

rvrs :: String
rvrs =
  concat [
    sAwesome,
    " ",
    sIs,
    " ",
    sCurry
  ]
  where
  sCurry   = take 5 string
  sIs      = take 2 . drop (5 + 1) $ string
  sAwesome = drop (8 + 1) string
  string   = "Curry is awesome"

{-

6. Put rvrs into a module
   I skipped this because it seemed too basic to me personally.



## 3.9 Definitions

* a string is a sequence of characters
* in Haskell String is represented by a linked-list of Char values AKA [Char]
* a type (AKA datatype) is a classification of values (AKA data)
* unlike other languages Haskell types do not delimit the operations that can beperformed on that data
* we sometimes say a value "inhabits" the type of ...
* concatenation means linking together sequences of values

* "scope"
* where a variable referred to be name (AKA identifier) is valid
* AKA "visibility"

* "local bindings"
* bindings that are child to some parent expression
* different from "top level" bindings in that they are not visible module-wide and cannot be exported and thus cannot be imported by external modules

* "top-level bidning"
* bindings outside of any other declaration
* can be shared across module and/or with other external modules

* "data structures"
* different ways of organizing data for convenience and performance according each according to different use-case(s)
-}
