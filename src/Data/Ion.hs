{-# LANGUAGE TemplateHaskell, LambdaCase #-}

{-|
Module      : Data.Ion
Description : Generate amazing, uncurried data-constructors from type-constructors.
Copyright   : (c) Kove W. Ochre-Salter, 2018
License     : MIT
Maintainer  : kove.w.o.salter@gmail.com
Stability   : experimental
Portability : TemplateHaskell, LambdaCase

= Description
 This module contains functions for generating “Ion”s for all of the data-constructors of a type-constructor.
 “Ion”s are, simply put, uncurried data-constructors.

 So the “Ion”s of 'Bool' are:

 @
 trueIon :: () -> Bool
 trueIon () = True

 falseIon :: () -> Bool
 falseIon () = False
 @

 The “Ion”s of 'Maybe' are:

 @
 justIon :: a -> Maybe a
 justIon = Just

 nothingIon :: () -> Maybe a
 nothingIon () = Nothing
 @

 and the “Ion”s of:

 @
 data Person = Person String Int
 @

 are:

 @
 personIon :: (String, Int) -> Person
 personIon (name, age) = Person name age
 @

 So, when you run:

 @
 mkIons ''Foo
 @

 a function that takes a tuple and returns a “Foo”, will be generated for each data-constructor of “Foo”’s.

= Examples
 /NOTE: all of the following examples require the “TemplateHaskell” language extension to work./

 == 1

  @
  import Prelude hiding (Bool)

  data Bool = True | False
 
  mkIons ''Bool
  @

  This will generate the following functions:

  @
  trueIon :: () -> Bool
  trueIon () = True

  falseIon :: () -> Bool
  falseIon () = False
  @

 == 2

  @
  data Shape = Circle Float | Rectangle Float Float Float | Triangle Float Float
  
  mkIons ''Shape
  @

  This will generate the following functions:

  @
  circleIon :: Float -> Shape
  circleIon = Circle

  rectangleIon :: (Float, Float, Float) -> Shape
  rectangleIon (l, d, h) = Rectangle l d h

  triangleIon :: (Float, Float) -> Shape
  triangleIon (b, h) = Triangle b h
  @

 = Naming
  “Ion” is an acronym for “amazing uncurrIed data-cONstructors”. I chose this name because,
  as we all know, ions are atoms (or groups of atoms) that are electrically charged,
  electricity is often associated with excitement and I felt excited about the concept of
  uncurried data-constructors, therefore, it is the perfect name.
-}

module Data.Ion (mkIons)
  where

    import Prelude hiding (error)

    import Language.Haskell.TH

    import Data.Char (toLower)
    import Data.List (span)

    import Control.Monad (replicateM)


    {-
     --------------------------------------------------------------------------------
                                         Primaries
     --------------------------------------------------------------------------------
     -}


    {-|
     = Description
      Generate an Ion for each, data-constructor belonging to a type-constructor with the 'Name' “typeName”.
      See the module-description for information.

     = Examples

      /See the module-description/.
     -}

    mkIons :: Name -> Q [Dec]
    mkIons typeName =
      reify typeName >>=
        \case
          TyConI dec -> case dec of
                          DataD _ _ _ _ cons _ -> mapM mkIon cons
                          _                    -> error "bad type-constructor"
          _          -> error "bad type-constructor"


    {-
     --------------------------------------------------------------------------------
                                          Helpers
     --------------------------------------------------------------------------------
     -}


    {-|
     = Description

      Generate an Ion for a data-constructor (the first argument).
      See the module-description for information.

     = Examples

      /Unavailable/.
     -}

    mkIon :: Con -> Q Dec
    mkIon = \case
              NormalC name bangTypes ->
                do names <- replicateM (length bangTypes) (newName "x")
                   return $ FunD name' [
                              Clause [TupP $ map VarP names]
                                     (NormalB $ applyE (ConE name) (map VarE names))
                                     []
                            ]
                where
                  name' = mkName ((uncapitalise $ last $ segmentate '.' (show name)) ++ "Ion")
                  applyE = foldl AppE
              RecC name varBangTypes -> mkIon (NormalC name bangTypes)
                where
                  bangTypes = map (\(_, b, t) -> (b, t)) varBangTypes
              _                      -> error "bad data-constructor"


    {-|
     = Description
      Generate a compile-time error message in the format:

      @
      Ion: ${message}.
      @

     = Examples

      /Unavailable/.
     -}

    error :: Monad m => String -> m a
    error message = fail $ concat [
                             "Ion: ",
                             message,
                             "."
                           ]


    {-|
     = Description

      Given some type “a” (the separator) and a list with elements of the type “a”,
      split the list into a list of segments separated by elements equal to the
      separator (the separator element is not included in the resulting list).

     = Examples

      == 1

       To split a qualified identifier into module levels do:
  
       >>> segmentate '.' "Data.Char.toUpper"
       ["Data", "Char", "toUpper"]
 
      == 2
 
       You can use it to split UNIX-style $PATHs (Windows style
       “\\”s have been used because of Haddock’s italic syntax):
  
       >>> segmentate ':' "\\bin:\\usr\\bin:\\usr\\local\\bin\\:."
       ["\\bin", "\\usr\\bin", "\\usr", "\\local\\bin\\", "."]

      == 2
 
       To define 'Data.List.words' do:
  
       >>> words' = segmentate ' '
       >>> words' "Hello, World"
       ["Hello,", "World"]

      == 3

       To define 'Data.List.lines' do:

       >>> lines' = segmentate '\n'
       >>> lines' "Hello\nWorld"
       ["Hello", "World"]
     -}

    segmentate :: Eq a => a -> [a] -> [[a]]
    segmentate _ []     = []
    segmentate x (y:ys) | y == x    = segmentate x ys
                        | otherwise = [y:ys'] ++ segmentate x ys''
                          where
                            (ys', ys'') = span (/=x) ys


    {-|
     = Description

      Convert the first character in a string to lower-case.

     = Examples

      == 1

       >>> uncapitalise "Foo"
       "foo"

      == 2

       >>> uncapitalise "FOO"
       "fOO"

      == 3

       >>> uncapitalise "_FOO"
       "_FOO"
     -}

    uncapitalise :: String -> String
    uncapitalise []     = []
    uncapitalise (c:cs) = toLower c : cs
