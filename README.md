# data-ion
![data-ion logo](./data-ion.png)

Generate amazing, uncurried data-constructors from type-constructors.

## Description
 This module contains functions for generating `Ion`s for all of the data-constructors of a type-constructor.
 `Ion`s are, simply put, uncurried data-constructors.

 So the Ions of `Bool` are:

 ```haskell
 trueIon () = True
 falseIon () = False
 ```

 The `Ion`s of `Maybe` are:

 ```haskell
 justIon = Just
 nothingIon () = Nothing
 ```

 and the “Ion”s of:

 ```haskell
 data Person = Person String Int
 ```

 are:

 ```haskell
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
  data Boolean = True | False
 
  mkIons ''Boolean
  ```
  This will generate the following functions:
  ```haskell
  trueIon () = True
  falseIon () = False
  ```
 2.
  ```haskell
  data Shape = Circle { radius :: Float }
             | Rectangle { length :: Float, depth :: Float, height :: Float }
             | Triangle { base :: Float, height_ :: Float }
  
  mkIons ''Shape
  ```
  This will generate the following functions:

  ```haskell
  circleIon = Circle
  rectangleIon (l, d, h) = Rectangle l d h
  triangleIon (b, h) = Triangle b h
  ```

## Naming
 `Ion` is an acronym for `amazing uncurrIed data-cONstructors`.
 I chose this name because, as we all know, ions are atoms (or groups of atoms) that are electrically charged, electricity is often associated with excitement and I felt excited about the concept of uncurried data-constructors, therefore, it is the perfect name.

## License
 `data-ion` is licensed under the MIT License.
 Please see [LICENSE.md](./LICENSE.md) for details.