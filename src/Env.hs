{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Env where

import Types
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.List ( delete )

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Linear.V2 (V2(..), _x, _y)
import qualified Data.Map as Map


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
  MaybeT (Just <$> modify moveButtonPlatform)


moveButtonPlatform :: Game -> Game
moveButtonPlatform g@Game {_buttons = b} = do
  let elsa_coord = toGridCoord(getCoord g 'e')
  let olaf_coord = toGridCoord(getCoord g 'o')
  let elsa_on_button = elsa_coord `Map.member` (g ^. buttons);
  let olaf_on_button = olaf_coord `Map.member` (g ^. buttons);

  -- Case where both elsa and olaf are on different buttons:
  --    1. Move platform cooresponding to elsa's button
  --    2. Move platform corresponding to olaf's button
  --    3. Move all other platforms passively
  if elsa_on_button && olaf_on_button && not (elsa_coord == olaf_coord)
    then
      moveAllPlatformsPassively (movePlatformOnButtonPress (movePlatformOnButtonPress g elsa_coord) olaf_coord) [elsa_coord, olaf_coord]
    else
      -- Case where only elsa is on a button and olaf is not:
      --    1. Move platform cooresponding to elsa's button
      --    2. Move all other platforms passively
      if elsa_on_button && not olaf_on_button
        then
          moveAllPlatformsPassively (movePlatformOnButtonPress g elsa_coord) [elsa_coord]
        else
          -- Case where only olaf is on a button (elsa is not on a button or they are on the same button)
          --    1. Move platform cooresponding to olaf's button
          --    2. Move all other platforms passively
          if olaf_on_button
            then
              moveAllPlatformsPassively (movePlatformOnButtonPress g olaf_coord) [olaf_coord]
            else
              -- Case where neither character is on a button
              --    1. Move all platforms passively
              moveAllPlatformsPassively g []

movePlatformOnButtonPress :: Game -> GridCoord -> Game
movePlatformOnButtonPress g@Game {_buttons = b} c@(V2 x y) =
  let 
    buttonPlatform = Map.findWithDefault (error "movePlatform: the button data should always be found") c b;
    initLoc = _platform_loc_init buttonPlatform;
    endLoc = _platform_loc_end buttonPlatform;
    (V2 curX curY) = _platform_loc buttonPlatform;
    direction = platformDirection initLoc endLoc;
    nextY = if curY == endLoc ^. _y then curY else curY + direction;
    nextLoc = (V2 curX nextY);
    newPlatform = ButtonPlatform {_platform_loc_init = initLoc, _platform_loc_end = endLoc, _platform_loc = nextLoc};
    g' = updateCharacterLocOnButtonPlatformCollision g nextLoc direction
  in
    g' & buttons .~ (Map.insert c newPlatform b)


updateCharacterLocOnButtonPlatformCollision :: Game -> GridCoord -> Int -> Game
updateCharacterLocOnButtonPlatformCollision g@Game { _elsa = e, _olaf = o } platform_loc direction =
  let 
    elsa_coord = toGridCoord(getCoord g 'e');
    olaf_coord = toGridCoord(getCoord g 'o');
    platform_locs = getAllGridCoordsForButtonPlatform platform_loc;
    collide_elsa = elsa_coord `elem` platform_locs;
    collide_olaf = olaf_coord `elem` platform_locs;
  in
    if collide_elsa && collide_olaf
      then
        let
          new_elsa = moveCharacterForButtonPlatformCollision e direction
          new_olaf = moveCharacterForButtonPlatformCollision o direction
        in
          (g & elsa .~ new_elsa) & olaf .~ new_olaf
      else
        if collide_elsa
          then 
            g & elsa .~ (moveCharacterForButtonPlatformCollision e direction)
          else
            if collide_olaf
              then
                g & olaf .~ (moveCharacterForButtonPlatformCollision o direction)
              else
                g

    

moveCharacterForButtonPlatformCollision :: Character -> Int -> Character
moveCharacterForButtonPlatformCollision c@Character {_loc = (V2 x y)} 1    = c & loc .~ (V2 x (y+1) :: PreciseCoord)
moveCharacterForButtonPlatformCollision c@Character {_loc = (V2 x y)} (-1) = c & loc .~ (V2 x (y-1) :: PreciseCoord)
moveCharacterForButtonPlatformCollision _ _ = error "moveCharacterForButtonPlatformCollision: Should only have arguments 1 and -1 for movement"


moveAllPlatformsPassively :: Game -> [GridCoord] -> Game
moveAllPlatformsPassively g@Game {_buttons = b} pressed_coords =
  let
    unactivePlatforms = Map.toList $ Map.filterWithKey (\k _ -> not (k `elem` pressed_coords)) b
  in
    foldl movePlatformPassively g unactivePlatforms


movePlatformPassively :: Game -> (GridCoord, ButtonPlatform) -> Game
movePlatformPassively g@Game {_buttons = b} (c, bp) =
  let
    initLoc = _platform_loc_init bp;
    endLoc = _platform_loc_end bp;
    (V2 curX curY) = _platform_loc bp;
    direction = platformDirection initLoc endLoc;
    nextY = if curY == initLoc ^. _y then curY else curY - direction;
    nextLoc = (V2 curX nextY);
    newPlatform = ButtonPlatform {_platform_loc_init = initLoc, _platform_loc_end = endLoc, _platform_loc = nextLoc}
    g' = updateCharacterLocOnButtonPlatformCollision g nextLoc (direction * (-1))
  in
    g' & buttons .~ (Map.insert c newPlatform b)


platformDirection :: GridCoord -> GridCoord -> Int
platformDirection (V2 _ y1) (V2 _ y2) =
  if y1 < y2
    then
      1
    else
      (-1)


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
    if (V2 x_val y_val) `elem` p || (V2 x_val y_val) `elem` getButtonPlatformLocs g
      then
        y_val + 1
      else
        (-1)

-- collisionCand returns a Bool depending on if a candidate location would cause a collision
collisionCand :: GridCoord -> Game -> Bool
collisionCand cand@(V2 x y) g@Game {_platform = p} = 
    if cand `elem` p || cand `elem` getButtonPlatformLocs g
      then
        True
      else
        False

collisionV :: GridCoord -> Game -> Bool
collisionV (V2 x y) g@Game {_platform = p} = 
  let 
    hi_c = (V2 x (y+1))
  in
    if hi_c `elem` p || hi_c `elem` getButtonPlatformLocs g
      then
        True
      else
        False


collisionH :: GridCoord -> Game -> Direction -> Int -> Bool
collisionH (V2 x y) g@Game {_platform = p, _buttons = b} d hv =
  let
    button_platform_locs = getButtonPlatformLocs g
    left_c  = (V2 (x-1) y);
    right_c = (V2 (x+1) y);
    mid = div maxSpeed 2;
    right_collide = right_c `elem` p || right_c `elem` button_platform_locs
    left_collide = left_c `elem` p || left_c `elem` button_platform_locs
  in
    case d of
      -- collides if moving in direction of obstacle
      RightDir -> right_collide
      LeftDir  -> left_collide

      -- collides if character will run into obstacle
      Neutral  -> if hv > mid then
          right_collide
        else if hv < mid then
          left_collide
        else
          False

      -- cannot collide horizontally when moving down
      DownDir  -> False
      

getAllGridCoordsForButtonPlatform :: GridCoord -> [GridCoord]
getAllGridCoordsForButtonPlatform p@(V2 x y) = [p, V2 (x+1) y, V2 (x+2) y]


getButtonPlatformLocs :: Game -> [GridCoord]
getButtonPlatformLocs g@Game {_buttons = b} = 
  let
    button_platform_locs = map (\x -> _platform_loc $ snd x) $ Map.toList b;
  in
    concat $ map getAllGridCoordsForButtonPlatform button_platform_locs

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

boundaryPlatforms :: [GridCoord]
boundaryPlatforms = ground ++ wall_l ++ wall_r ++ ceiling
  where
    ground  = [V2 x 0 | x <- [(-1)..width]]
    wall_l  = [V2 (-1) y | y <- [1..height]]
    wall_r  = [V2 width y | y <- [1..height]]
    ceiling = [V2 x height | x <- [0..(width-1)]] 

-- Initialize game with token and character locations
-- NOTE: lakes are platforms too. They're just scary platforms that can kill you :)  -- NZ
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _elsa = Character {
            _loc = initLocE
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = initLocO
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = initTokensE
        , _tokensO  = initTokensO
        , _exits = initExits
        , _platform = initLakesE ++ initLakesO ++ initDeathLakes ++ initPlatform ++ boundaryPlatforms
        , _lakesE = initLakesE ++ initDeathLakes
        , _lakesO = initLakesO ++ initDeathLakes
        , _deathLakes = initDeathLakes
        , _buttons = Map.fromList buttonPlatforms
        , _jump = False
        , _dead   = False
        , _done   = False
        }
  return $ execState gameState g


buttonPlatforms :: [(GridCoord, ButtonPlatform)]
buttonPlatforms = 
  [
    ((V2 49 18),  ButtonPlatform {_platform_loc_init = (V2 37 19), _platform_loc_end = (V2 37 45), _platform_loc = (V2 37 19)})
  , ((V2 33 43), ButtonPlatform {_platform_loc_init = (V2 45 19), _platform_loc_end = (V2 45 42), _platform_loc = (V2 45 19)})
  ]

initLocE :: PreciseCoord
initLocE = V2 0.0 1.0

initLocO :: PreciseCoord
initLocO = V2 1.0 1.0

initTokensO :: [GridCoord]
initTokensO = [V2 10 1, V2 12 1, V2 14 1, V2 33 1, V2 35 1, V2 38 1, V2 24 13, V2 32 24, V2 30 47, V2 23 47, V2 17 47, V2 9 47]
-- initTokensE = [V2 3 9, V2 5 10, V2 10 10, V2 15 12]

initTokensE :: [GridCoord]
initTokensE = [V2 10 5, V2 12 5, V2 14 5, V2 33 5, V2 35 5, V2 38 5, V2 37 12, V2 14 10, V2 15 22, V2 27 46, V2 20 46, V2 13 46]
-- initTokensO = [V2 35 4, V2 28 1]

initExits :: [GridCoord]
initExits = [V2 34 29, V2 34 34]
-- initExits = [V2 25 1, V2 26 1]

initDeathLakes :: [GridCoord]
initDeathLakes = [V2 22 0, V2 23 0, V2 24 0, V2 14 17, V2 15 17, V2 16 17, V2 17 17, V2 28 17, V2 29 17, V2 30 17, V2 31 17, V2 32 17, V2 33 17, V2 0 28, V2 1 28, V2 2 28, V2 3 28, V2 4 28, V2 5 28, V2 6 28, V2 7 28, V2 8 28, V2 9 28, V2 10 28]
-- initDeathLakes = [V2 18 14]

initLakesO :: [GridCoord]
initLakesO = [V2 7 4, V2 8 4, V2 9 4, V2 10 4, V2 11 4, V2 12 4, V2 13 4, V2 14 4, V2 15 4, V2 16 4, V2 17 4, V2 31 4, V2 32 4, V2 33 4, V2 34 4, V2 35 4, V2 36 4, V2 37 4, V2 38 4, V2 39 4, V2 40 4, V2 41 4, V2 42 4, V2 35 9, V2 36 9, V2 37 9, V2 38 9, V2 39 9, V2 13 34, V2 14 34, V2 34 28]
-- initLakesE = [V2 7 0, V2 8 0, V2 9 0, V2 18 0, V2 19 0] ++ [V2 30 10, V2 31 10, V2 32 10, V2 33 10]

initLakesE :: [GridCoord]
initLakesE = [V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0, V2 40 0, V2 41 0, V2 42 0, V2 19 31, V2 20 31, V2 32 33, V2 33 33]
-- initLakesO = [V2 30 0, V2 31 0, V2 32 0] ++ [V2 20 15, V2 21 15, V2 22 16]

initPlatform :: [GridCoord]
initPlatform = [V2 31 28, V2 32 28, V2 33 28, V2 40 42, V2 41 42, V2 42 42, V2 43 42, V2 11 28, V2 12 28, V2 13 28, V2 14 28, V2 15 28, V2 16 28, V2 17 28, V2 18 28, V2 19 28, V2 20 28, V2 21 28, V2 22 28, V2 10 17, V2 11 17, V2 12 17, V2 13 17, V2 22 9, V2 23 9, V2 24 9, V2 25 9, V2 26 9, V2 18 17, V2 19 17, V2 20 17, V2 21 17, V2 14 9, V2 15 9, V2 34 17, V2 35 17, V2 36 17, V2 37 17, V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 6 4, V2 18 4, V2 30 4, V2 43 4, V2 43 0, V2 44 0, V2 45 0, V2 46 0, V2 47 0, V2 48 0, V2 49 0, V2 48 1, V2 49 1, V2 48 2, V2 49 2, V2 48 3, V2 49 3, V2 48 4, V2 49 4, V2 0 9, V2 1 9, V2 2 9, V2 3 9, V2 4 9, V2 5 9, V2 6 9, V2 7 9, V2 8 9, V2 9 9, V2 10 9, V2 11 9, V2 12 9, V2 13 9, V2 16 9, V2 17 9, V2 18 9, V2 19 9, V2 20 9, V2 21 9, V2 27 9, V2 28 9, V2 29 9, V2 30 9, V2 31 9, V2 32 9, V2 33 9, V2 34 9, V2 40 9, V2 41 9, V2 42 9, V2 43 9, V2 44 9, V2 45 9, V2 0 10, V2 1 10, V2 2 10, V2 3 10, V2 4 10, V2 0 11, V2 1 11, V2 2 11, V2 3 11, V2 0 12, V2 1 12, V2 2 12, V2 0 13, V2 1 13, V2 0 14, V2 1 14, V2 4 17, V2 5 17, V2 6 17, V2 7 17, V2 8 17, V2 9 17, V2 22 17, V2 23 17, V2 24 17, V2 25 17, V2 26 17, V2 27 17, V2 38 17, V2 39 17, V2 40 17, V2 41 17, V2 42 17, V2 43 17, V2 44 17, V2 45 17, V2 46 17, V2 47 17, V2 48 17, V2 49 17, V2 22 18, V2 23 18, V2 24 18, V2 25 18, V2 23 19, V2 24 19, V2 25 19, V2 14 21, V2 15 21, V2 16 21, V2 17 21, V2 30 20, V2 31 20, V2 32 20, V2 33 20, V2 34 20, V2 4 42, V2 5 42, V2 6 42, V2 7 42, V2 8 42, V2 9 42, V2 10 42, V2 11 42, V2 12 42, V2 13 42, V2 14 42, V2 15 42, V2 16 42, V2 17 42, V2 18 42, V2 19 42, V2 20 42, V2 21 42, V2 22 42, V2 23 42, V2 24 42, V2 25 42, V2 26 42, V2 27 42, V2 28 42, V2 29 42, V2 30 42, V2 31 42, V2 32 42, V2 33 42, V2 34 42, V2 35 42, V2 1 38, V2 2 38, V2 3 38, V2 4 38, V2 5 38, V2 6 38, V2 7 38, V2 8 38, V2 8 34, V2 9 34, V2 10 34, V2 11 34, V2 12 34, V2 14 31, V2 15 31, V2 16 31, V2 17 31, V2 18 31, V2 23 28, V2 24 28, V2 25 28, V2 26 28, V2 27 28, V2 28 28, V2 29 28, V2 30 28, V2 34 28, V2 34 33, V2 35 28, V2 35 29, V2 35 30, V2 35 31, V2 35 32, V2 35 33, V2 35 34, V2 35 35, V2 35 36, V2 35 37, V2 35 38, V2 35 39, V2 35 40, V2 35 41, V2 35 42]
-- initPlatform = [V2 2 2, V2 3 3, V2 4 3, V2 5 3, V2 6 3, V2 9 6, V2 10 6, V2 11 6, V2 12 6, V2 16 10, V2 17 10, V2 18 10, V2 43 2, V2 43 1] ++ [ V2 (-1) (-1), V2 (-1) 0, V2 0 (-1), V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0, V2 40 0, V2 41 0, V2 42 0, V2 43 0, V2 44 0, V2 45 0, V2 46 0, V2 47 0, V2 48 0, V2 49 0]
