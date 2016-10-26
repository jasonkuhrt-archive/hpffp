
module Chapter1 where

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



## 1.5 Beta reduction

* "beta reduction"
* the process of applying an abstraction to an input
* replace all instances of bound variables within body with input expression
* eliminate head (its only purpose was to bind a variable AKA parameter)
* recursive process, stops when any of: no more heads (AKA lambdas), no more inputs to apply functions to

* application in lambda calculus is left-associative
* this means application associates or groups from and toward the left
* so (𝝺x.x)(𝝺v.v)(𝝺z.z)a   ==   ( ( (𝝺x.x)(𝝺v.v) )(𝝺z.z) ) a

* "free variables"
* in body but not bound by head

* a computation is an initial lambda expression (or two if separating initial input) and a finite sequence of lambda terms each deduced from the proceeding term by one application of beta reduction.

* alpha equivalence does not apply to free variables



## 1.6 Multiple arguments

* strictly speaking a function can only have one input
* functions that require multiple inputs achieve this via nested heads
* Discovered by Moses Schönfinkel 1920s, rediscovered by Haskell Curry
* when a multi-input function is applied to an input the outer most head is eliminated (and as always the variables in body bound to the parameter in head being eliminated are replaced with the value), leaving the next head, and so on
* e.g.:

𝝺xvz.xvz   ==   𝝺x.(𝝺v.(𝝺z.xvz))

* in academia it is common to use abstract variables rather than concrete values
* names of variables have no significance in and of themselves, what is important however is understanding the head whose parameter binds that name

* example beta reduction of (𝝺xyz.xz(yz))(𝝺mn.m)(𝝺p.p)

** Explicit currying
   (𝝺x.𝝺y.𝝺z.xz(yz)) (𝝺m.𝝺n.m) (𝝺p.p)
** Explicit grouping
   ((𝝺x.𝝺y.𝝺z.xz(yz)) (𝝺m.𝝺n.m)) (𝝺p.p)
** beta reduce step, (𝝺m.𝝺n.m) -> x
   (𝝺y.𝝺z.(𝝺m.𝝺n.m)z(yz)) (𝝺p.p)
** beta reduce step, (𝝺p.p) -> y
   𝝺z.(𝝺m.𝝺n.m)z((𝝺p.p)z)
** beta reduce step, z -> m
   𝝺z.(𝝺n.z)((𝝺p.p)z)
** beta reduce step, ((𝝺p.p)z) -> n, tossed, no n binding in body
   𝝺z.z

* observe above how beta reduction works from outer-most left-most

### Exercises: Equivalence

1. Answer B
   𝝺xy.xz   ==   𝝺mn.mz
   Alpha Equivalence. The names do not matter just their pattern.
2. Answer C
   𝝺xy.xxy   ==   𝝺a(𝝺b.aab)
2. Answer B
   𝝺xyz.zx   ==   𝝺tos.st



## 1.7 Evaluation is simplification

* "beta normal form"
* when you cannot reduce the terms any further (AKA evaluate, AKA apply lambdas to arguments)
* the reduced form of an expression is its normal form
* e.g. 2 is the normal form of 2000 / 1000

"saturated"
* a function with all parameters applied

* a saturated lambda not simplified to final result is therefore not fully evaluated–merely applied.

* ... application is what makes evaluation/simplification possible

* e.g. NOT in beta normal form
  (𝝺x.x)z

* e.g. IN beta normal form
  z
  𝝺x.x



## 1.8 Combinators

* "combinator"
* a lambda term with no free variables and serve only to combine the arguments they are given



## 1.9 Divergence

* "divergence"
* lambda terms that whose beta reduction never terminates

* typically beta reduction converges to beta normal form
* terms that diverge are terms that do no produce an answer/meaningful result
* understanding what will terminate means understanding what will do useful work



## 1.10 Summary

* functions have head and body
* functions have one input and produce one output
* functions always produce the same output for the same input
* functions map an input set to an output set
* variables may be bound in a function head
* every variable instance in function body is the same value
* FP is based on three kinds of expressions: variables, functions, expressions combined with other expressions
* haskell has lots of sugar but semantically and at its core is a lambda calculus
* haskell is actually a _typed_ lambda calculus



## 1.11 Chapter Exercises

### Are these combinators?

1. 𝝺x.xxx
   YES

2. 𝝺xy.zx
   NO z is a free variable

3. 𝝺xyz.xy(zx)
   YES

4. 𝝺xyz.xy(zxy)
   YES

5. 𝝺xy.xy(zxy)
   NO z is a free variables

### Normal form or diverge?

1. 𝝺x.xxx
   is normal form (cannot be further reduced)

2. (𝝺z.zz)(𝝺y.yy)
   diverges, this is omega

3. (𝝺x.xxx)z
   has normal form zzz

### Beta reduce

1.  (𝝺abc.cba)zz(𝝺wv.w)
    ((𝝺abc.cba)zz)(𝝺wv.w)
    ((𝝺[a:=z]bc.cba)z)(𝝺wv.w)
    ((𝝺bc.cbz)z)(𝝺wv.w)
    (𝝺[b:=z]c.cbz)(𝝺wv.w)
    (𝝺c.czz)(𝝺wv.w)
    (𝝺[c:=(𝝺wv.w)].czz)
    (𝝺wv.w)zz
    ((𝝺wv.w)z)z
    (𝝺[w:=z]v.w)z
    (𝝺v.z)z
    𝝺[v:=z].z
    z

2.  (𝝺x.𝝺y.xyy)(𝝺a.a)b
    ((𝝺xy.(xy)y)(𝝺a.a))b
    (𝝺[x:=(𝝺a.a)]y.(xy)y)b
    (𝝺y.((𝝺a.a)y)y)b
    𝝺[y:=b].((𝝺a.a)y)y
    ((𝝺a.a)b)b
    (𝝺[a:=b].a)b
    (b)b
    bb

3.  (𝝺y.y)(𝝺x.xx)(𝝺z.zq)
    ((𝝺y.y)(𝝺x.xx))(𝝺z.zq)
    (𝝺[y:=(𝝺x.xx)].y)(𝝺z.zq)
    (𝝺x.xx)(𝝺z.zq)
    (𝝺[x:=(𝝺z.zq)].xx)
    (𝝺z.zq)(𝝺z.zq)
    𝝺[z:=(𝝺z.zq)].zq
    (𝝺z.zq)q
    𝝺[z:=q].zq
    qq

4.  (𝝺z.z)(𝝺z.zz)(𝝺z.zy)
    ((𝝺z.z)(𝝺z.zz))(𝝺z.zy)
    (𝝺[z:=(𝝺z.zz)].z)(𝝺z.zy)
    (𝝺z.zz)(𝝺z.zy)
    (𝝺[z:=(𝝺z.zy)].zz)
    (𝝺z.zy)(𝝺z.zy)
    (𝝺[z:=(𝝺z.zy)].zy)
    (𝝺z.zy)y
    𝝺[z:=y].zy
    yy

5.  (𝝺x.𝝺y.xyy)(𝝺y.y)y
    ((𝝺xy.(xy)y)(𝝺y.y))y
    (𝝺[x:=(𝝺y.y)]y.(xy)y)y
    (𝝺y.((𝝺y.y)y)y)y
    𝝺[y:=y].((𝝺y.y)y)y
    ((𝝺y.y)y)y
    (𝝺[y:=y].y)y
    (y)y
    yy

6.  (𝝺a.aa)(𝝺b.ba)c
    ((𝝺a.aa)(𝝺b.ba))c
    ((𝝺[a:=(𝝺b.ba)].aa))c
    ((𝝺b.ba)(𝝺b.ba))c
    (𝝺[b:=(𝝺b.ba)].ba)c
    ((𝝺b.ba)a)c
    (𝝺[b:=a].ba)c
    (aa)c
    aac

7.  (𝝺xyz.xz(yz)) (𝝺x.z) (𝝺x.a)

    x := (𝝺x.z1)                   <--  tricky step [1]
    (𝝺yz.(𝝺x.z1) z (yz)) (𝝺x.a)

    y := (𝝺x.a)
    𝝺z.(𝝺x.z1) z ((𝝺x.a)z)

    x := z
    𝝺z.z1 ((𝝺x.a)z)

    x := z
    𝝺z.z1 a

    𝝺z.z1a

* [1] Remap name of free variable z to z1 so that it does not conflict with z of the first lambda whose body this function expression is about to be inserted into. Alternatively one could just as well remap the name of the bound variable involved in the conflict.
-}
