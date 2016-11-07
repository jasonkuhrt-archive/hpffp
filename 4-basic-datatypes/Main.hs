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



## 4.3 Anatomy of a Data Declaration

* "data declaration"
* how a datatype is defined

* "type constructor"
* name of a type
* syntactically, always capitalized
* appear only in type-level code (code following the `::`)

* "data constructor"
* values that inhabit the type they are defined in
* appear only in term-level code

* "term level"
* values as they appear in code or that code evaluates to

* "Bool"
* a basic datatype in Haskell
* named after the logician George Boole
* example:
-}

data Boolean = FALSE | TRUE -- <-- a data declaration
--   ^         ^     ^
--   |         |     | "sum type" AKA logical disjunction, or
--   |         | data constructor for value FALSE, term-level name
--   | type constructor for datatype Boolean, type-level name

{-

* data constructors and type constructors can have arguments
* data declarations can model logical conjunctions (AKA and)
* use ghci `:info` on data constructors to read their datatypes declartions (filtered to just that data constructor )
* use ghci `:info` on datatypes to read their declarations

* example of term-level / type-level distinction
  * type-level
    :type not
    Bool -> Bool
  * term-level
    not True

### Intermission Exercises: Mood Swing
-}

data Mood = Blah | Woot deriving Show

{-

1. What is the type constructor (AKA name) of this type?
   Mood
2. If a function requires a Mood value what values could possibly be used?
   Blah, Woot

3. We want to write a changeMood function. What's wrong with the initial attempt of changeMood :: Mood -> Woot?

   Initial attempt fails because it uses data constructors (Woot) at the type-level

4. Write that function correctly now
-}

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

{-

5. Try it in ghci
   OK!
-}
