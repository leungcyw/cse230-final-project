{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MyCharacter
  ( initGame
  , step
  , move
  , Game(..)
  , Direction(..)
  , dead, tokens, exits, myCharacter, done
  , height, width
  ) where
import Types
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.List ( delete )

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Linear.V2 (V2(..), _x, _y)

makeLenses ''Game

-- | Step triggered by action
step :: Direction -> Game -> Game
step d s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use dead, use done]

  MaybeT (Just <$> modify (move d)) -- move
  MaybeT (Just <$> modify eatToken) -- check for token
  -- MaybeT (Just <$> modify die) -- check if dead
  MaybeT (Just <$> modify checkDone) -- check if done

-- Die if next position is in wrong lake
-- die :: MaybeT (State Game) ()
-- die = do
--   MaybeT . fmap guard $ elem <$> (getCoord <$> get) <*> use myCharacter
--   MaybeT . fmap Just $ dead .= True

-- Done if tokens are gone and in front of exit door
checkDone :: Game -> Game
checkDone g@Game { _tokens = t, _exits = e, _done = d } = do
  if null t && (getCoord g `elem` e)
    then
      g & done .~ True
    else
      g

-- Eat token if current position == token
eatToken :: Game -> Game
eatToken g@Game { _tokens = t } = do
  if getCoord g `elem` t
    then
      g & tokens .~ delete (getCoord g) t
    else
      g

-- Represents current game state
gameState :: State Game ()
gameState = do
  return ()

-- Move charcter in necessary direction
move :: Direction -> Game -> Game
move d g@Game { _myCharacter = s } = g & myCharacter .~ nextPos d g

-- Finds next position of character based on direction
nextPos :: Direction -> Game -> Coord
nextPos d Game { _myCharacter = a }
  | d == UpDir = a & _y %~ (\y -> if (y + 1) < height then y + 1 else y)
  | d == DownDir = a & _y %~ (\y -> if (y - 1) >= 0 then y - 1 else y)
  | d == RightDir = a & _x %~ (\x -> if (x + 1) < width then x + 1 else x)
  | d == LeftDir = a & _x %~ (\x -> if (x - 1) >= 0 then x - 1 else x)
nextPos _ _ = error "invalid direction??"

-- Get current coordinates of character
getCoord :: Game -> Coord
getCoord Game { _dir = d, _myCharacter = a } = a

-- Initialize game with token and character locations
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _myCharacter  = V2 0 0
        , _tokens  = [V2 5 3, V2 10 3]
        , _exits = [V2 20 3]
        , _dir    = UpDir
        , _dead   = False
        , _done   = False
        }
  return $ execState gameState g