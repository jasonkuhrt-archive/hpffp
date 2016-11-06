module Chapter4 where

{-

## 4.1 Basic Datatypes

* Haskell has a robust type system
* types help with:
  * readability
  * safety
  * maintainability – classified data is easier to systematically restructure later
* types AKA datatypes
* types permit solving problems with _less_ code
* concepts include:
  * typeclasses
  * type constructors
  * data types
  * data constructors



## 4.2 What Are Types?

* evaluated expressions reduce to values (AKA redexes)
* every value has a type
* types are used to group values that have something in common (AKA types are classifications, categories, groups, ..., of values)
* "something in common" may be an abstract idea, or specific model to a particular domain
* thinking about types like mathematical sets will help guide intuition on how types work in a mathematical sense
  * set theory is the study of mathematical collections of objects
  * set theory was a precursor to type theory
  * set theory ideas like disjunction (or), conjunction (and) used to manipulate sets have equivalents in Haskell type system
-}
