# data-ion

A little play with Template Haskell! I do not think that there are really any applications for this.

## Description
 `data-ion` is a library containing functions for generating `Ion`s for all
 of the data-constructors of a type-constructor. `Ion`s are, simply put,
 uncurried data-constructors.

 So the `Ion`s of `Bool` are:

 ```haskell
 trueIon :: () -> Bool
 trueIon () = True

 falseIon :: () -> Bool
 falseIon () = False
 ```

 The `Ion`s of `Maybe` are:

 ```haskell
 justIon :: a -> Maybe a
 justIon = Just

 nothingIon :: () -> Maybe a
 nothingIon () = Nothing
 ```

 and the `Ion`s of:

 ```haskell
 data Person = Person String Int
 ```

 are:

 ```haskell
 personIon :: (String, Int) -> Person
 personIon (name, age) = Person name age
 ```

 So, when you run:

 ```haskell
 mkIons ''Foo
 ```

 a function that takes a tuple and returns a `Foo`, will be generated for each data-constructor of `Foo`’s.

## Examples
 *NOTE: all of the following examples require the `TemplateHaskell` language extension to work.*

 1.
  ```haskell
  {-# LANGUAGE TemplateHaskell #-}

  import Prelude hiding (Bool)

  data Bool = True | False
 
  mkIons ''Bool
  ```
  This will generate the following functions:
  ```haskell
  trueIon :: () -> Bool
  trueIon () = True

  falseIon :: () -> Bool
  falseIon () = False
  ```
 2.
  ```haskell
  {-# LANGUAGE TemplateHaskell #-}

  data Shape = Circle Float | Rectangle Float Float Float | Triangle Float Float
  
  mkIons ''Shape
  ```
  This will generate the following functions:

  ```haskell
  circleIon :: Float -> Shape
  circleIon = Circle

  rectangleIon :: (Float, Float, Float) -> Shape
  rectangleIon (l, d, h) = Rectangle l d h

  triangleIon :: (Float, Float) -> Shape
  triangleIon (b, h) = Triangle b h
  ```

## License
 `data-ion` is licensed under the MIT License.
 Please see [LICENSE](./LICENSE) for details.
