{-# LANGUAGE TemplateHaskell, OverloadedStrings, ExistentialQuantification #-}

module Main
  where

    import Data.Ion
    import Data.Text (Text)

    data Person = Person {
      personFirstName :: Text,
      personLastName :: Text,
      personAge :: Int
    } deriving Show

    data Pet = Pet {
      petName :: Text,
      petHeight :: Float,
      petAge :: Int,
      petType :: Type
    } deriving Show

    data Type = Dog | Cat | Bird
      deriving Show

    data Test = forall a. Show a => Test a

    mkIons ''Person
    mkIons ''Pet

    tests = [
      Test $ personIon ("Mr. Foo", "Bar", 0),
      Test $ personIon ("Ms. Qux", "Bar", 0),
      Test $ petIon ("Jack", 3.1415927, 0, Dog)
     ]

    test n (Test s) =
      do putStrLn ("  + " ++ show n ++ ")")
         putStrLn ("    " ++ show s)

    runTests tests' =
      mapM (uncurry test) (zip [0..] tests')

    main =
      do putStrLn "<====== Begin Tests ======>"
         runTests tests
         putStrLn "<====== End Tests   ======>"
