
module Chapter2 where

{-# ANN module "HLint: ignore Redundant bracket" #-}

{-
# Chapter 2 – Hello, Haskell!



## 2.1 Hello, Haskell

* use stack to install haskell, haskellstack.org
* includes ghci (repl), ghc (haskell), and build tool
* brew install haskell-stack
* another guide to getting up and running with haskell is available at https://github.com/bitemyapp/learnhaskell



## 2.2 Interacting with Haskell code

* two ways of working with code
* typing directly into ghci, a repl (read-eval-print-loop)
* loading a source file into ghci
* repl originated with lisp
* `ghci` command available if haskell installed standalone
* otherwise must use `stack ghci`
* Prelude is a library of standard functions
* automatically imported
* can be disabled e.g. for an alternative
* Prelude library is within package `base`
* ghci has special commands prefixed by `:` like :info :quit :type
-}

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

{-
* `::` denotes a type
* can be read as "... has the type of ..."
* in ghci use :load to load a file, path is relative to `pwd`
* in ghci use :module to unload the loaded module



## 2.3 Understanding expressions

* everything is an expression or declaration
* expressions may be values, combinations of values, functions applied to values
* expressions evaluate to a result
* expressions are building blocks
* a program is one giant expression

* "declaration"
* top-level binding that permit naming expressions
* example expressions:

  1
  1 + 1
  "Icarus"
* 1 is an expression, has no further reduction step, "stands for itself"

"normal form"
* an expression for which no more evaluation steps may be taken
* AKA reached an irreducible form

"redex"
* a reducible expression

"evaluation"
* AKA reduction
* AKA normalizing, but imprecise
* AKA executing, but imprecise



## 2.4 Functions

"function" in Haskell
* based on the mathematical function
* an expression that is applied to a (one) argument, returns a (one) result
* maps inputs to outputs

"argument"
* value passed to function parameter when function is applied

* b/c built purely of expressions always evaluate to the same result when given the same values
* multi-parameter functions use currying
* functions can be an argument to another function or nested in a function body
* to introduce functions in ghci `let` must be used (not needed in a source file) e.g.

  let triple x = x * 3
      ^      ^   ^^^^^
      name   |       |
             param   |
                     body

* Once a function is applied you can replace it with its body whose bound variables are now replaced by the arguments
-}

triple :: Int -> Int
triple x = x * 3

{-
## 2.5 Evaluation

* "evaluation"
* reduce the terms of an expression until the expression reaches its simplest form
* usually the result is called a value
* AKA application since applying a function to an argument allows evaluation

* Haskell uses a lazy evaluation strategy
* terms are not evaluated until they are forced to by another term referring to them
* this is to say that by default Haskell does not evaluate to normal form, but rather weak head normal form (WHNF)
* approximated example:

  (\ x -> (1, 2 + x)) 2

evaluation result in `normal order`: `(1, 4)`
evaluation result in WHNF:           `(1, 2 + 2)`

### Exercises: Comprehension Check

1. Rewrite these to make them work in ghci:

  half x = x / 2
  let half x = x / 2

  square x = x * x
  let square x = x * x

2. Write function that can work for all the following expressions:
  3.14 * (5 * 5)
  3.14 * (10 * 10)
  3.14 * (2 * 2)
  3.14 * (4 * 4)
-}

timesMiniPi :: Float -> Float
timesMiniPi n = n * 3.14

{-
3. Rewrite using `pi`
-}

timesPi :: Float -> Float
timesPi n = n * pi

{-
## 2.6 Infix operators

"prefix syntax"
* default for functions
* means function being applied is at the beginning of the expression rather than e.g. the middle

"operator"
* function that can be used infix style

* all operators are functions, not vice versa
* functions can be used infix style by wrapping them with backticks
* operators can be used function style by wrapping them with parentheses

* :info command in ghci can show info about functions including their:
  * associativity
  * precedence
  * is operator or not
* e.g.:

  :info (*)
  infixl  7  *
  [1]    [2][3]

* [1] is an infix operator, left associative
* [2] precedence, scale 0-9, higher is applied first
* [3] infix name

"left associative operator"
* means `2 * 3 * 4` is same as `(2 * 3) * 4`

"right associative operator"
* means `2 ^ 3 ^ 4` is same as `2 ^ (3 ^ 4)`
* means `2 ^ 3 ^ 4` (241...) is not same as `(2 ^ 3) ^ 4` (4096)

### Exercises: Parentheses and Association

Are parentheses needed in the following expressions?

1. (8 + 7) * 9
   YES, * takes precedence otherwise

2. perimeter x y = (x * 2) + (y * 2)
   NO, * is higher precedence anyways

3. f x = x / (2 + 9)
   YES, / takes precedence otherwise



## 2.7 Declaring values

* declare a module name so that it can be imported
* syntax is whitespace sensitive
* general rule is that expressions wrapped onto the next line need to indent at least one character, then stay at that indent level e.g.:

  -- error

  f1 = 'a'
  == 'b'

  -- ok
-}

f1 :: Bool
f1 = 'a'
  == 'b'

{-
* Examples with let ... in syntax:
-}

f2 :: [String]
f2 =
  let
    -- `foo` and `bar` must line up
    foo = "foo"
    bar = "bar"
  in
    -- `qux` and `wux` must line up
    let qux = "qux"
        wux = "wux"
    in
    let
    -- `fuf` and `bux` must line up
    fuf = "fuf"
    bux = "bux"
    in
    [foo, bar, qux, wux, fuf, bux] {-
    or even either:
      [foo, bar, qux, wux, fuf, bux]
  [foo, bar, qux, wux, fuf, bux] -}

{-

### Intermission Exercises

Fix whitespace or other errors.

1. let area x = 3. 14 * (x * x)

   period in 3. 14 interpreted as composition operator
   fix:

   let area x = 3.14 * (x * x)

2. let double x = b * 2

   b is undefined
   fix:

   let double x = x * 2

3. x = 7
    y = 10
   f = x + y

   y indented too much
   fix:

   x = 7
   y = 10
   f = x + y



## 2.8 Arithmetic functions in Haskell

* div and quot have different rounding logic
* `/` operator uses typeclasses
* there is a law that each quot/rem and div/mod operations pair follow
* the pairings hint at the said differences of rounding between div and quot
* the law is: given a numerator and denominator value, apply dividing function, multiply result by denominator value, add result of applying remainder function, equals numerator
* notationally:
-}

type Numerator   = Int
type Denominator = Int

lawQuotRem :: Numerator -> Denominator -> Bool
lawQuotRem n d = ((quot n d) * d) + (rem n d) == n

{-
* the law's pattern is the same for the div/mod pair:
-}

lawDivMod :: Numerator -> Denominator -> Bool
lawDivMod n d = ((div n d) * d) + (mod n d) == n

{-
* the law shows a relationship.
* a different rounding mechanism in regards to two groups of integral division
* each group having a division operation and remainder operation
* combining a pair of these operations (from the same group) effectively undoes the division.
* the basic idea seems to be:
  * multiplication undoes the _integer_ divison, meaning except what was rounded
  * adding the remainder undoes just the division's rounding
* another law (not sure this is interesting, since its the already established semantic differentiation of the pairings) is regarding the remainder of the division operation always being equal to the remainder operation
-}

law2DivMod :: Numerator -> Denominator -> Bool
law2DivMod n d = (n - (div n d) * d) == (mod n d)

{-
* "mod"
* operation for modular arithmetic
* gives remainder of modular division

* "modular arithmetic"
* system of arithmetic for integers where numbers "wrap around" upon reaching a certain value, called the modulus
* e.g. in a 12 hour clock we wrap counting around the 12 e.g. 8am + 8h is not 16 o'clock but rather 4 o'clock
* this example would be called arithmetic modulo 12
* in modulo n, n is equal to itself and zero
* in modular division the denominator is the modulus
* e.g. clocks are modulo 12: 11:00pm + 1h is 00:00am
* e.g. months are modulo 12: Dec (11) + 1m is Jan (0)
* e.g. days of week are modulo 7: Sun (6) + 1d is Mon (0)

* modular arithmetic often gives same answer as arithmetic
* however three examples of differences:

  * if the denominator is a divisor of the numerator (evenly divides it) then the result is 0, since numerator wrapping around denominator finishes at denominator AKA the modulus and the modulus is equal to 0

    mod 10 5 == 0
    div 10 5 == 2

  * if numerator is lower than denominator then result is numerator

    mod 10 12 == 10
    div 10 12 == 0

  * result has same sign as denominator, whereas with rem result has same sign as numerator.
  * this behaviour is particular to mod in Haskell, not mod in all languages

    mod 11 (-5) == (-4) -- (wraps backwards)
    rem 11 (-5) == 1
    rem (-11) 5 == (-1)
    mod (-11) 5 == 4 -- (wraps backwards)
    mod (-11) (-5) == (-1)

  * if xor numerator/denominator negative then:
    multiple of denominator closest to numerator on side **farthest** from zero (X), subtract X from numerator

    mod (-10) 6 ==   2   -- -10 - -12 ==  2
    mod 10 (-6) == (-2)  --  10 -  12 == -2
                              ^    ^
                              |    | closest multiple on side farthest from zero
                              | numerator

  * otherwise:
    multiple of denominator closest to numerator on side **closest** to zero (X), subtract X from numerator

    mod (-10) (-6) == (-4) -- -10 - -6 == -4
    mod   10    6  ==   4  --  10 -  6 ==  4
                                ^    ^
                                |    | closest multiple on side closest to zero
                                | numerator

* book suggests that mod takes more experience than div to know when to use

### Negative Numbers

* negative numbers on their own can be written as e.g. `-10`
* what is actually happening is that `-` is a unary operator
* negative numbers used in expressions does not work the same
* e.g.: `5 + -10` does not work because haskell now reads `-` not as a negate operator but as a subtraction operator
* `5 + (-10)` or `5 + (negate 10)` does work
* so in Haskell the `-` operator is overloaded
* overloaded operators in Haskell are an exception



## 2.9 Parenthesization

* `$` is defined with lowest possible precedence (0)
* very commonly used in idiomatic Haskell code

* operators can be used prefix-style if wrapped in `()`

  (*) 2 2

* operators can be referred as value if wrapped in `()`

  (fmap (*) [2,2]) <*> [2,2]

* "sectioning"
* applying either-or operator's first or second param

  (^2), (2/), ...

* subtraction is special case, following does not work:

  (-2) 1

  because it is interpreted as negative 2 applied to 1
-}
