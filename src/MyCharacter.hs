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
  , lakesE, elsa, lakesO, olaf
  , dead, tokens, exits, done
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
  MaybeT (Just <$> modify die) -- check if dead
  MaybeT (Just <$> modify checkDone) -- check if done

die :: Game -> Game
die g@Game { _lakesE = l, _lakesO = o } = do
  if (toGridCoord(getCoord g 'e') `elem` l || toGridCoord(getCoord g 'o') `elem` o)
    then
      g & dead .~ True
    else
      g

-- Done if tokens are gone and in front of exit door
checkDone :: Game -> Game
checkDone g@Game { _tokens = t, _exits = e, _done = d } = do
  if null t && (toGridCoord(getCoord g 'e') `elem` e) && (toGridCoord(getCoord g 'o') `elem` e)
    then
      g & done .~ True
    else
      g

-- Eat token if current position == token
eatToken :: Game -> Game
eatToken g@Game { _tokens = t } = do
  if (toGridCoord (getCoord g 'e')) `elem` t
    then
      g & tokens .~ delete (toGridCoord (getCoord g 'e')) t
    else
      g


canJump :: Character -> Bool
canJump c@Character {_loc = l} =
  let
    coord = toGridCoord l;
    y_val = coord ^. _y
  in
    if y_val == 0    -- TODO: change this check to check both x and y coords for platforms
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
move d j g@Game { _elsa = s } = g & elsa .~ nextPos d j g

-- Finds next position of character based on direction
nextPos :: Direction -> Bool -> Game -> Character
nextPos d j g@Game { _elsa = a} =
  do
    let new_c = nextVv d j (nextHv d a)
    let h_cand = ((a & _loc) ^. _x) + (speedTable !! (new_c & _hv))
    let v_cand = ((a & _loc) ^. _y) + (speedTable !! (new_c & _vv))
    let new_h
          | h_cand < 0 = 0
          | h_cand > fromIntegral width - 1 = fromIntegral width - 1
          | otherwise = h_cand
    let new_v
          | v_cand < 0 = 0
          | v_cand > fromIntegral height - 1 = fromIntegral height - 1
          | otherwise = v_cand

    new_c & loc .~ (V2 new_h new_v :: PreciseCoord)

-- Get next horizontal velocity from the table
nextHv :: Direction -> Character -> Character
nextHv d c@Character { _hv = h}
  | d == LeftDir = c & hv .~ div maxSpeed 10
  | d == RightDir = c & hv .~ round (fromIntegral(maxSpeed - 1) * 0.9)
  | d == DownDir = c & hv .~ div maxSpeed 2
  | d == Neutral = if h < div maxSpeed 2 then c & hv .~ h + 1 else if h > div maxSpeed 2 then c & hv .~ h - 1 else c
  | otherwise = error "invalid dir"

-- Non dashing version, will need adjustments with a different acceleration curve if using
-- | d == LeftDir = if h > 0 then c & hv .~ h - 1 else c
-- | d == RightDir = if h < (maxSpeed - 1) then c & hv .~ h + 1 else c

-- Get next vertical velocity from the table
nextVv :: Direction -> Bool -> Character -> Character
nextVv d j c@Character { _vv = v}
  | d == DownDir = c & vv .~ 0
  | canJump c && j = c & vv .~ (maxSpeed - 1)
  | v > 0 = c & vv .~ v - 1
  | otherwise = c

-- Get current coordinates of character
getCoord :: Game -> Char -> PreciseCoord
getCoord Game { _dir = d, _elsa = a } 'e' = a & _loc
getCoord Game { _dir = d, _olaf = a } 'o' = a & _loc

-- Initialize game with token and character locations
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _elsa = Character {
            _loc = V2 0.0 0.0
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = V2 49.0 0.0
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokens  = [V2 3 0, V2 5 2, V2 10 2, V2 15 2]
        , _exits = [V2 25 0]
        , _lakesE = [V2 7 0, V2 8 0, V2 9 0, V2 18 0, V2 19 0]
        , _lakesO = [V2 40 0, V2 41 0, V2 42 0]
        , _dir = Neutral
        , _jump = False
        , _dead   = False
        , _done   = False
        }
  return $ execState gameState g