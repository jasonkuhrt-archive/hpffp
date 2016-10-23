
main :: IO ()
main = undefined

{-
# Chapter 1 – All You Need is Lambda



## 1.1 All you need is lambda

* "lambda calculus" (LC)
* a model of computation
* developed in 1930s
* By Alonzo Church

* "calculus"
* a method of calculation or reasoning

* LC formalizes the concept of effective computability
* means it determines which problems (or classes of problems) can be solved via computation.
* Haskell is built on LC



## 1.2 What is functional programming?

* "functional programming" (FP)
* a programming paradigm relying on functions modeled on mathematical functions
* programs are a combination of "expressions"
* based on lambda calculus
* some FP languages have features that are not translatable into a lambda expression

* "pure functional language" (PFP)
* A language solely expressed in terms that translate into lambda expressions
* for example Haskell

* "expression"
* concrete values, variables, functions

* "function"
* the definition of a relation between a set of possible inputs to outputs
* an expression applied to an input (AKA argument)
* once applied can be evaluated (AKA reduced)
* are first-class

* "first-class"
* something that can be a value or given as an input

* "pure"
* casual terminology for formal term "referential transparency"

* "Referential Transparency" (RT)
* a function given the same inputs will always evalute to the same output
* is how functions work in math

* "abstraction"
* write shoter more concise programs by factoring common repeated structures into more generic code that can be reused



## 1.3 What is a function?

* "domain"
* the set of a function's inputs

* "codomain"
* the set of a function's _unique_ outputs

* "range"
* the set of a function's possible outputs _as related to different inputs_
* unlike codomain this set may have values repeated since two different inputs may result in the same output
* e.g. various inputs 9,8,7... given to function `(< 10)` (is less than 10) all produce the same output

* given f (< 10)
* codomain {True, False}
* range {..., 9->True, 10->False, ...} (infinite: all integers below 10 mapping to true, all integers above 9 mapping to false)

* `f` is RT because the same output is always returned for the same input

f(1) = A
f(2) = B
f(3) = C
f(4) = D
f(5) = D
f(6) = D

* `f` is _not_ RT because input 1 maps to output A _OR_ B?!

f(1) = A
f(1) = B

* remember that a non RT "function" is an invalid mathematical function and since Haskell is a PLP it is an invalid Haskell function too
* thinking of functions as a mapping of inputs to outputs is crucial to being effective with FP



## 1.4 The structure of lambda terms

* three basic components: variables, expressions, abstractions
* expression is a superset of variables and expressions

* "expression"
* variable
* abstraction
* combination thereof

* "variable"
* names for potential inputs to functions
* no meaning or value

* "abstraction"
* a function
* has a head, body
* applied to an input
* e.g.  𝝺n.n

* "head"
* symbol "𝝺" followed by a parameter

"body"
* an expression

"parameter"
* a variable in head
* binds all instances of variable found in body to the represented value (the value that the parameter represents)

"alpha equivalence"
* the idea that 𝝺x.x and 𝝺v.v mean the same thing since the variable is not semantically meaningful
-}
