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

 So the Ions of 'Bool' are:

 > trueIon = True
 > falseIon = False

 The “Ion”s of 'Maybe' are:

 > justIon = Just
 > nothingIon = Nothing

 and the “Ion”s of:

 > data Person = Person String Int deriving (Show)

 are:

 > personIon (name, age) = Person name age

 So, when you run:

 @
 mkIons ''Foo
 @

 a function that takes a tuple and returns a “Foo”, will be generated for each data-constructor of “Foo”’s.

= Examples
 /NOTE: all of the following examples require the “TemplateHaskell” language extension to work./

 == 1

  > data Boolean = True | False
  >
  > mkIons ''Boolean

  This will generate the following functions:

  > trueIon () = True
  > falseIon () = False

 == 2

  > data Shape = Circle { radius :: Float } | Rectangle { length :: Float, depth :: Float, height :: Float } | Triangle { base :: Float, height_ :: Float }
  >
  > mkIons ''Shape

  This will generate the following functions:

  > circleIon = Circle
  > rectangleIon (l, d, h) = Rectangle l d h
  > triangleIon (b, h) = Triangle b h

 = Naming
 “Ion” is an acronym for “amazing uncurrIed data-cONstructors”.
 I chose this name because, as we all know, ions are atoms (or groups of atoms) that are electrically charged,
 electricity is often associated with excitement and I felt excited about the concept of uncurried data-constructors,
 therefore, it is the perfect name.
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
     Generate an Ion for each, data-constructor belonging to a type-constructor with the 'Name' “typeName”.
     See the module-description for information.
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
     Generate an Ion for a data-constructor (the first argument).
     See the module-description for information.
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
     Generate a compile-time error message in the format:

     @
     Ion: ${message}.
     @
     -}

    error :: Monad m => String -> m a
    error message = fail $ concat [
                             "Ion: ",
                             message,
                             "."
                           ]


    {-|
     Split a qualified-name into a list of 'Strings' contain each module-level of the name.
     
     For example:

     >>> segmentate "Data.Char.toUpper"
     ["Data", "Char", "toUpper"]
     -}

    segmentate :: Eq a => a -> [a] -> [[a]]
    segmentate _ []     = []
    segmentate x (y:ys) | y == x    = segmentate x ys
                        | otherwise = [y:ys'] ++ segmentate x ys''
                          where
                            (ys', ys'') = span (/=x) ys


    {-|
     Convert the first character in a string to lower-case.

     For example:

     >>> uncapitalise "Foo"
     "foo"
     -}

    uncapitalise :: String -> String
    uncapitalise []     = []
    uncapitalise (c:cs) = toLower c : cs
