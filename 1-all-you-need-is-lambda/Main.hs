
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
-}
