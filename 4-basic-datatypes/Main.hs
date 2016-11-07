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



## 4.4 Numeric Types

* Haskell does not just use one type of number

* "integral numbers"
* whole numbers positive and negative
* type `Int`
  * fixed precision, meaning it has a min/max
  * in other words does not support arbitrarily small/large numbers
* type `Integer`
  * does support arbitrarily large/small numbers

* "fractional"
* not integers
* type `Float`
  * single-precision floating point numbers
  * can shift how many bits used to represent numbers before/after decimal point
  * flexible but floating point arithmetic violates some common assumptions
  * only use with great care, probably should not be used in business applications
  * maybe useful for graphics programming e.g. OpenGL
* type `Double`
  * double-precision floating point number
  * has twice as many bits as Float to describe numbers
* type `Rational`
  * fractional number that describes ratio between two integers
  * e.g. 1 / 2 :: Rational will be a value carrying two integer values: numerator 1 and denominator 2, a ratio of 1:2
  * arbitrarily precise
  * not as effecient as Scientific
* type `Scientific`
  * space effecient and almost-arbirarily precise scientific number
  * represented using scientific notation
  * stores coefficient as an integer
  * stores expoent as an Int
  * the limit on expoent technically limits the range of Scientific but in practice its unlikely to hit that limit
  * not part of `base`, available in a library, package `scientific`
  * much more effecient than rational

* all numeric datatypes are instances of Num typeclass
* `Num` typeclass is what provides operators like (+) (*) etc.
* typeclasses are a way of adding functionality to types that is reusable across all datatypes that have instances of that typeclass
* a typeclass instance defines how a particular datatype works for the methods declared by the typeclass

* interestingly data constructors for numeric types are not written out because most numeric types are infinite sets of values

* type `Int` is an artifact of what computer hardware has supported natively over the years
* most programs should use `Integer` rather than `Int` unless you know otherwise for performance or some such
* the danger of Int et al (Int8, Int16, the number of bits the type uses to represent the number) is the ability to have numbers go out of range
* the representation used for fixed-size Int types is two's complement

* operator `/` requires arguments to be instances of a typeclass Fractional
* Fractional typeclass requires types already have an instance of Num typeclass
  * this relation is said as Num is a superclass of Fractional
* values of `Fractional a => a` default to `Double` type
* `Scientific` is usually preferable
* most people find floating point arithmetic hard to reason about
* in order to avoid using mistakes use Scientific by default



## 4.5 Comparing Values

* typeclass `Eq` concerns things that can be compared for equivilance
* typeclass `Ord` concerns things that can be ordered
* Char implements `Ord` therefore you can do e.g. `'a' > 'c'`
* Char order is lexicographic (unicode order) so `'a' > 'A' == True`
* a list can be checked against another list only if the contained datatype implements `Ord`

  e.g. ERROR [Blah, Woot] > [Woot]
  e.g. FINE  [1, 2] > [1]

* the following is an error

  True 'a'

  becuase `==` is polymorphic over one type not two and so once given `True` requires that the next argument value must also inhabit `Bool`
-}
