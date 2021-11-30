{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Env
  ( initGame
  , step
  -- , noActionStep
  , move
  , Game(..)
  , Direction(..)
  , Character(..)
  , lakesE, elsa, lakesO, olaf, platform
  , dead, tokensE, tokensO, exits, done
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
step :: Char -> Direction -> Bool -> Game -> Game
step c d j s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use dead, use done]

  MaybeT (Just <$> modify (move c d j)) -- move
  MaybeT (Just <$> modify eatTokenE)    -- check for token
  MaybeT (Just <$> modify eatTokenO)    -- check for token
  MaybeT (Just <$> modify die)          -- check if dead
  MaybeT (Just <$> modify checkDone)    -- check if done

die :: Game -> Game
die g@Game { _lakesE = e, _lakesO = o } = do
  let elsa_coord = toGridCoord(getCoord g 'e')
  let olaf_coord = toGridCoord(getCoord g 'o')
  if (nextToBadLake elsa_coord e || nextToBadLake olaf_coord o)
    then
      g & dead .~ True
    else
      g

nextToBadLake :: GridCoord -> [GridCoord] -> Bool
nextToBadLake c cs = 
  let
    x_coord = c ^. _x;
    y_coord = c ^. _y;
    c1'     = V2 (x_coord + 1) y_coord;
    c2'     = V2 (x_coord - 1) y_coord;
    c3'     = V2 x_coord (y_coord + 1);
    c4'     = V2 x_coord (y_coord - 1)
  in
    if c1' `elem` cs || c2' `elem` cs || c3' `elem` cs || c4' `elem` cs
      then
        True
      else
        False

-- Done if tokens are gone and in front of exit door
checkDone :: Game -> Game
checkDone g@Game { _tokensE = t1, _tokensO = t2, _exits = e, _done = d } = do
  if null t1 && null t2 && (toGridCoord(getCoord g 'e') `elem` e) && (toGridCoord(getCoord g 'o') `elem` e)
    then
      g & done .~ True
    else
      g

-- Eat token if current position == token
eatTokenE :: Game -> Game
eatTokenE g@Game { _tokensE = t } = do
  if (toGridCoord (getCoord g 'e')) `elem` t
    then
      g & tokensE .~ delete (toGridCoord (getCoord g 'e')) t
    else
      g

eatTokenO :: Game -> Game
eatTokenO g@Game { _tokensO = t } = do
  if (toGridCoord (getCoord g 'o')) `elem` t
    then
      g & tokensO .~ delete (toGridCoord (getCoord g 'o')) t
    else
      g


-- onPlatform returns the y coordinate of the platform if the character is on a platform and -1 otherwise
onPlatform :: Character -> Game -> Int
onPlatform c@Character {_loc = l} g@Game {_platform = p} =
  let
    coord = toGridCoord l;
    y_val = (coord ^. _y) - 1; -- subtract 1 to check if character is on top of a platform
    x_val = coord ^. _x
  in
    if (V2 x_val y_val) `elem` p   
      then
        y_val + 1
      else
        (-1)

-- collisionCand returns a Bool depending on if a candidate location would cause a collision
collisionCand :: GridCoord -> Game -> Bool
collisionCand cand@(V2 x y) g@Game {_platform = p} = 
    if cand `elem` p
      then
        True
      else
        False

collisionV :: GridCoord -> Game -> Bool
collisionV (V2 x y) g@Game {_platform = p} = 
  let 
    hi_c = (V2 x (y+1))
  in
    if hi_c `elem` p
      then
        True
      else
        False


collisionH :: GridCoord -> Game -> Direction -> Int -> Bool
collisionH (V2 x y) g@Game {_platform = p} d hv =
  let
    left_c  = (V2 (x-1) y);
    right_c = (V2 (x+1) y);
    mid = div maxSpeed 2
  in
    case d of
      -- collides if moving in direction of obstacle
      RightDir -> if right_c `elem` p then True else False
      LeftDir  -> if left_c `elem` p then True else False

      -- collides if character will run into obstacle
      Neutral  -> if hv > mid then
          right_c `elem` p
        else if hv < mid then
          left_c `elem` p
        else
          False

      -- cannot collide horizontally when moving down
      DownDir  -> False
      


-- Represents current game state
gameState :: State Game ()
gameState = do
  return ()

-- Move charcter in necessary direction
move :: Char -> Direction -> Bool -> Game -> Game
move c Neutral False g@Game { _elsa = e, _olaf = o } = g & (elsa .~ nextPos e Neutral False g) & (olaf .~ nextPos o Neutral False g)
move c d j g@Game { _elsa = e, _olaf = o }
  | c == 'e' = g & elsa .~ nextPos e d j g
  | c == 'o' = g & olaf .~ nextPos o d j g

-- Finds next position of character based on direction
nextPos :: Character -> Direction -> Bool -> Game -> Character
nextPos c@Character {_hv = hv, _loc = l} d j g@Game {} =
  do
    let collision_v  = collisionV (toGridCoord l) g
    let collision_h  = collisionH (toGridCoord l) g d hv
    let new_c        = nextVv d j (nextHv d c collision_h) g collision_v
    let h_cand       = ((c & _loc) ^. _x) + (speedTable !! (new_c & _hv))
    let v_cand       = ((c & _loc) ^. _y) + (speedTable !! (new_c & _vv))
    let cand_collision = collisionCand (toGridCoord (V2 h_cand v_cand)) g
    let platform_res = onPlatform c g
    let new_h
          | h_cand < 0 = 0                                             -- min horizontal position is 0
          | h_cand > fromIntegral width - 1 = fromIntegral width - 1   -- max horizontal position is (width - 1)
          | otherwise = h_cand                                         -- default: move to candidate horizontal position
    let new_v
          | v_cand > fromIntegral height - 1 = fromIntegral height - 1 -- max vertical position is (height - 1)
          | cand_collision && not (platform_res == (-1)) = fromIntegral platform_res      -- vertical position is platform if on platform and not jumping
          | cand_collision && (platform_res == (-1)) = ((c & _loc) ^. _y)   -- vertical position is on top of platform if colliding diagonally
          | otherwise = v_cand                                         -- default: move to candidate vertical position

    new_c & loc .~ (V2 new_h new_v :: PreciseCoord)

-- Get next horizontal velocity from the table
nextHv :: Direction -> Character -> Bool -> Character
nextHv d c@Character { _hv = h} collision
    -- set horizontal velocity to 0 for horizontal collision
  | collision = c & hv .~ div maxSpeed 2
    -- set horizontal velocities to their respective directions for left and right
  | d == LeftDir = c & hv .~ div maxSpeed 10
  | d == RightDir = c & hv .~ round (fromIntegral(maxSpeed - 1) * 0.9)
    -- set horizontal velocity to 0 when falling straight down
  | d == DownDir = c & hv .~ div maxSpeed 2
    -- keep going in same direction if no key input
  | d == Neutral = if h < div maxSpeed 2 then c & hv .~ h + 1 else if h > div maxSpeed 2 then c & hv .~ h - 1 else c
  | otherwise = error "invalid dir"

-- Non dashing version, will need adjustments with a different acceleration curve if using
-- | d == LeftDir = if h > 0 then c & hv .~ h - 1 else c
-- | d == RightDir = if h < (maxSpeed - 1) then c & hv .~ h + 1 else c

-- Get next vertical velocity from the table
nextVv :: Direction -> Bool -> Character -> Game -> Bool -> Character
nextVv d j c@Character { _vv = v} g collision
    -- if in the air and collide, then set vertical velocity to 0.25 downward velocity
  | collision && platform_val == (-1) = c & vv .~ div maxSpeed 4
    -- if vertical collision when trying to perform a valid jump, keep vertical velocity at 0
  | collision && platform_val > 0 && j = c & vv .~ div maxSpeed 2
    -- set vertical velocity to max downward on down input
  | d == DownDir = c & vv .~ 0
    -- set vertical velocity to max upward on valid jump
  | platform_val > 0 && j = c & vv .~ (maxSpeed - 1)
    -- if on the ground, constrain max downward velocity
  | platform_val > 0 && v > 0 = if v > div maxSpeed 4 then c & vv .~ v - 1 else c
    -- keep going through speed table
  | v > 0 = c & vv .~ v - 1
    -- default: no change in vertical velocity
  | otherwise = c
  where
    platform_val = onPlatform c g 

-- Get current coordinates of character
getCoord :: Game -> Char -> PreciseCoord
getCoord Game { _elsa = a } 'e' = a & _loc
getCoord Game { _olaf = a } 'o' = a & _loc


skyPlatforms :: [GridCoord]
skyPlatforms = [V2 2 2, V2 3 3, V2 4 3, V2 5 3, V2 6 3, V2 9 6, V2 10 6, V2 11 6, V2 12 6, V2 16 10, V2 17 10, V2 18 10, V2 43 2, V2 43 1]

skyLakesE :: [GridCoord]
skyLakesE = [V2 30 10, V2 31 10, V2 32 10, V2 33 10]

skyLakesO :: [GridCoord]
skyLakesO = [V2 20 15, V2 21 15, V2 22 16]

-- Initialize game with token and character locations
-- NOTE: lakes are platforms too. They're just scary platforms that can kill you :)  -- NZ
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _elsa = Character {
            _loc = V2 0.0 1.0
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = V2 49.0 1.0
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = [V2 3 9, V2 5 10, V2 10 10, V2 15 12]
        , _tokensO  = [V2 35 4, V2 28 1]
        , _exits = [V2 25 1, V2 26 1]
        , _platform = skyLakesE ++ skyLakesO ++ skyPlatforms ++ [ V2 (-1) (-1), V2 (-1) 0, V2 0 (-1), V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0, V2 40 0, V2 41 0, V2 42 0, V2 43 0, V2 44 0, V2 45 0, V2 46 0, V2 47 0, V2 48 0, V2 49 0]
        , _lakesE = [V2 7 0, V2 8 0, V2 9 0, V2 18 0, V2 19 0] ++ skyLakesE
        , _lakesO = [V2 30 0, V2 31 0, V2 32 0] ++ skyLakesO
        , _jump = False
        , _dead   = False
        , _done   = False
        }
  return $ execState gameState g