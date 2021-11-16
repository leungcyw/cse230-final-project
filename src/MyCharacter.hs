{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module MyCharacter
  ( initGame
  , step
  -- , noActionStep
  , move
  , Game(..)
  , Direction(..)
  , Character(..)
  , dead, tokens, exits, myCharacter, done
  , height, width
  , hv, vv, loc
  , toGridCoord
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
makeLenses ''Character

-- noActionStep :: Game -> Game
-- noActionStep s = flip execState s . runMaybeT $ do
--   MaybeT (Just <$> modify moveGravity) -- jump

toGridCoord :: PreciseCoord -> GridCoord
toGridCoord (V2 x_f y_f) = (V2 (floor x_f) (floor y_f))

-- | Step triggered by action
step :: Direction -> Bool -> Game -> Game
step d j s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use dead, use done]

  MaybeT (Just <$> modify (move d j)) -- move
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
  if null t && (toGridCoord(getCoord g) `elem` e)
    then
      g & done .~ True
    else
      g

-- Eat token if current position == token
eatToken :: Game -> Game
eatToken g@Game { _tokens = t } = do
  if (toGridCoord (getCoord g)) `elem` t
    then
      g & tokens .~ delete (toGridCoord (getCoord g)) t
    else
      g


canJump :: Character -> Bool
canJump c@Character {_loc = l} = 
  let
    coord = toGridCoord l;
    y_val = coord ^. _y
  in
    if (y_val == 0)    -- TODO: change this check to check both x and y coords for platforms
      then
        True
      else
        False

-- Represents current game state
gameState :: State Game ()
gameState = do
  return ()

-- Move charcter in necessary direction
move :: Direction -> Bool -> Game -> Game
move d j g@Game { _myCharacter = s } = g & myCharacter .~ nextPos d j g

-- Finds next position of character based on direction
nextPos :: Direction -> Bool -> Game -> Character
nextPos d j g@Game { _myCharacter = a} = 
  do
    let new_c = nextVv d j (nextHv d a)
    let h_cand = ((a & _loc) ^. _x) + (speedTable !! (new_c & _hv))
    let v_cand = ((a & _loc) ^. _y) + (speedTable !! (new_c & _vv))
    let new_h = if h_cand < 0 then 0 else if h_cand > fromIntegral width - 1 then fromIntegral width - 1 else h_cand
    let new_v = if v_cand < 0 then 0 else if v_cand > fromIntegral height - 1 then fromIntegral height - 1 else v_cand
    
    new_c & loc .~ ((V2 new_h new_v) :: PreciseCoord)

-- Get next horizontal velocity from the table
nextHv :: Direction -> Character -> Character
nextHv d c@Character { _hv = h}
  | d == LeftDir = c & hv .~ div maxSpeed 10
  | d == RightDir = c & hv .~ round (fromIntegral(maxSpeed - 1) * 0.9)
  | d == DownDir = c & hv .~ div maxSpeed 2
  | d == Neutral = if h < div maxSpeed 2 then c & hv .~ h + 1 else if h > div maxSpeed 2 then c & hv .~ h - 1 else c

-- Non dashing version, will need adjustments with a different acceleration curve if using
-- | d == LeftDir = if h > 0 then c & hv .~ h - 1 else c
-- | d == RightDir = if h < (maxSpeed - 1) then c & hv .~ h + 1 else c

-- Get next vertical velocity from the table
nextVv :: Direction -> Bool -> Character -> Character
nextVv d j c@Character { _vv = v} = 
  if d == DownDir then c & vv .~ 0 else if canJump c && j then c & vv .~ (maxSpeed - 1) else if v > 0 then c & vv .~ v - 1 else c

-- Get current coordinates of character
getCoord :: Game -> PreciseCoord
getCoord Game { _dir = d, _myCharacter = a } = a & _loc

-- Initialize game with token and character locations
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _myCharacter  = Character {
            _loc = V2 0.0 0.0
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokens  = [V2 3 0, V2 5 2, V2 10 2, V2 15 2]
        , _exits = [V2 20 0]
        , _dir = Neutral
        , _jump = False
        , _dead   = False
        , _done   = False
        }
  return $ execState gameState g