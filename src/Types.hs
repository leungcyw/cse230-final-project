module Types where

import Linear.V2 (V2(..), _x, _y)

-- Used in MyCharacter.hs
data Game = Game
  { _myCharacter  :: MyCharacter
  , _dir          :: Direction
  , _tokens       :: [Coord]
  , _exits        :: [Coord]
  , _dead         :: Bool
  , _done         :: Bool
  } deriving (Show)

type Coord = V2 Int

type MyCharacter = Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = UpDir
  | DownDir
  | RightDir
  | LeftDir
  deriving (Eq, Show)

height :: Int
height = 50

width :: Int
width = 50

-- Used in UI.hs
data Tick = Tick -- Can be used to mark time

type Name = ()

data Cell = MyCharacter | Token | Exit | Empty