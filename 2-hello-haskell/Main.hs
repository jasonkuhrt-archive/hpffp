main :: IO ()
main = undefined

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
-}
