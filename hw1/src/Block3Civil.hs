module Block3Civil
  ( Civilization (..)
  , Building (..)
  , Castle (..)
  , House (..)
  , buildCastle
  , buildChurchLib
  , buildNewHouse
  , lordToCastle
  , buildWalls
  ) where

import Data.List.NonEmpty (NonEmpty (..), toList)

data Building
  = Church
  | Library
  | NoBuild

data Castle
  = NoCastle
  | NewCastle Bool Bool

data House
  = One
  | Two
  | Three
  | Four

getPeople :: House -> Int
getPeople One   = 1
getPeople Two   = 2
getPeople Three = 3
getPeople Four  = 4

data Civilization = Civilization
  { getCastle  :: Castle
  , getChurLib :: Building
  , getHouses  :: NonEmpty House
  }

instance Show Civilization where
  show (Civilization c b h) = "Civilization: " ++ show c ++ "," ++ show b ++ "," ++ show (toList h)

instance Show Castle where
  show NoCastle = "no castle"
  show (NewCastle lord wall) | lord && wall = "castle with lord, walls"
                             | wall         = "castle, walls"
                             | lord         = "castle with lord"
                             | otherwise    = "castle"

instance Show Building where
  show Church  = "church"
  show Library = "library"
  show NoBuild = ""

instance Show House where
  show One   = "house with 1 person"
  show Two   = "house with 2 people"
  show Three = "house with 3 people"
  show Four  = "house with 4 people"

buildCastle :: Civilization -> Either String Civilization
buildCastle a = case getCastle a of
  NoCastle -> Right $ Civilization (NewCastle False False) (getChurLib a) (getHouses a)
  _        -> Left "Already has castle"

buildChurchLib :: Building -> Civilization -> Either String Civilization
buildChurchLib s civil = case getChurLib civil of
  Church  -> Left "Civilization has church"
  Library -> Left "Civilization has library"
  NoBuild -> Right $ Civilization (getCastle civil) s (getHouses civil)

buildNewHouse :: House -> Civilization -> Civilization
buildNewHouse k civil = Civilization (getCastle civil) (getChurLib civil) (k :| toList (getHouses civil))

lordToCastle :: Civilization -> Either String Civilization
lordToCastle civil = case getCastle civil of
  NoCastle -> Left "There is no castle"
  NewCastle a b -> if a then Left "There is another lord"
    else Right $ Civilization (NewCastle True b) (getChurLib civil) (getHouses civil)

buildWalls :: Civilization -> Either String Civilization
buildWalls civil = case getCastle civil of
  NoCastle -> Left "There is no castle!"
  NewCastle a _ | countPeople (toList (getHouses civil)) < 10 -> Left "Not enough people to build"
                | a -> Right $ Civilization (NewCastle a True) (getChurLib civil) (getHouses civil)
                | otherwise -> Left "There is no lord"
    where
      countPeople = foldr ((+) . getPeople) 0
