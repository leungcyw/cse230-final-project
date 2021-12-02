module Constants where

import Types
import qualified Data.Map as Map
import Linear.V2 (V2(..), _x, _y)

boundaryPlatforms :: [GridCoord]
boundaryPlatforms = ground ++ wall_l ++ wall_r ++ ceiling
  where
    ground  = [V2 x 0 | x <- [(-1)..width]]
    wall_l  = [V2 (-1) y | y <- [1..height]]
    wall_r  = [V2 width y | y <- [1..height]]
    ceiling = [V2 x height | x <- [0..(width-1)]] 

buttonPlatforms :: [(GridCoord, ButtonPlatform)]
buttonPlatforms = 
  [
    ((V2 3 1),  ButtonPlatform {_platform_loc_init = (V2 20 2), _platform_loc_end = (V2 20 7), _platform_loc = (V2 20 2)})
  , ((V2 25 1), ButtonPlatform {_platform_loc_init = (V2 5 26), _platform_loc_end = (V2 5 18), _platform_loc = (V2 5 26)})
  ]

initLocE :: PreciseCoord
initLocE = V2 0.0 1.0

initLocO :: PreciseCoord
initLocO = V2 1.0 1.0

initTokensO :: [GridCoord]
initTokensO = [V2 10 1, V2 12 1, V2 14 1, V2 33 1, V2 35 1, V2 38 1, V2 24 13, V2 32 24, V2 42 26, V2 46 41, V2 30 47, V2 23 47, V2 17 47, V2 9 47]
-- initTokensE = [V2 3 9, V2 5 10, V2 10 10, V2 15 12]

initTokensE :: [GridCoord]
initTokensE = [V2 10 5, V2 12 5, V2 14 5, V2 33 5, V2 35 5, V2 38 5, V2 37 12, V2 14 10, V2 15 22, V2 45 33, V2 39 38, V2 27 46, V2 20 46, V2 13 46]
-- initTokensO = [V2 35 4, V2 28 1]

initExits :: [GridCoord]
initExits = [V2 34 29, V2 34 34]
-- initExits = [V2 25 1, V2 26 1]

initDeathLakes :: [GridCoord]
initDeathLakes = [V2 10 17, V2 11 17, V2 12 17, V2 13 17, V2 14 17, V2 15 17, V2 16 17, V2 17 17, V2 28 17, V2 29 17, V2 30 17, V2 31 17, V2 32 17, V2 33 17, V2 0 28, V2 1 28, V2 2 28, V2 3 28, V2 4 28, V2 5 28, V2 6 28, V2 7 28, V2 8 28, V2 9 28, V2 10 28, V2 11 28, V2 12 28, V2 13 28, V2 14 28, V2 15 28, V2 16 28, V2 17 28, V2 18 28, V2 19 28, V2 20 28, V2 21 28, V2 22 28]
-- initDeathLakes = [V2 18 14]

initLakesO :: [GridCoord]
initLakesO = [V2 7 4, V2 8 4, V2 9 4, V2 10 4, V2 11 4, V2 12 4, V2 13 4, V2 14 4, V2 15 4, V2 16 4, V2 17 4, V2 31 4, V2 32 4, V2 33 4, V2 34 4, V2 35 4, V2 36 4, V2 37 4, V2 38 4, V2 39 4, V2 40 4, V2 41 4, V2 42 4, V2 35 9, V2 36 9, V2 37 9, V2 38 9, V2 39 9, V2 13 34, V2 14 34, V2 31 28, V2 32 28, V2 33 28]
-- initLakesE = [V2 7 0, V2 8 0, V2 9 0, V2 18 0, V2 19 0] ++ [V2 30 10, V2 31 10, V2 32 10, V2 33 10]

initLakesE :: [GridCoord]
initLakesE = [V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0, V2 40 0, V2 41 0, V2 42 0, V2 22 9, V2 23 9, V2 24 9, V2 25 9, V2 26 9, V2 19 31, V2 20 31, V2 32 33, V2 33 33]
-- initLakesO = [V2 30 0, V2 31 0, V2 32 0] ++ [V2 20 15, V2 21 15, V2 22 16]

initPlatform :: [GridCoord]
initPlatform = [V2 18 17, V2 19 17, V2 20 17, V2 21 17, V2 14 9, V2 15 9, V2 34 17, V2 35 17, V2 36 17, V2 37 17, V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 6 4, V2 18 4, V2 30 4, V2 43 4, V2 43 0, V2 44 0, V2 45 0, V2 46 0, V2 47 0, V2 48 0, V2 49 0, V2 48 1, V2 49 1, V2 48 2, V2 49 2, V2 48 3, V2 49 3, V2 48 4, V2 49 4, V2 0 9, V2 1 9, V2 2 9, V2 3 9, V2 4 9, V2 5 9, V2 6 9, V2 7 9, V2 8 9, V2 9 9, V2 10 9, V2 11 9, V2 12 9, V2 13 9, V2 16 9, V2 17 9, V2 18 9, V2 19 9, V2 20 9, V2 21 9, V2 27 9, V2 28 9, V2 29 9, V2 30 9, V2 31 9, V2 32 9, V2 33 9, V2 34 9, V2 40 9, V2 41 9, V2 42 9, V2 43 9, V2 44 9, V2 45 9, V2 0 10, V2 1 10, V2 2 10, V2 3 10, V2 4 10, V2 0 11, V2 1 11, V2 2 11, V2 3 11, V2 0 12, V2 1 12, V2 2 12, V2 0 13, V2 1 13, V2 0 14, V2 1 14, V2 4 17, V2 5 17, V2 6 17, V2 7 17, V2 8 17, V2 9 17, V2 22 17, V2 23 17, V2 24 17, V2 25 17, V2 26 17, V2 27 17, V2 38 17, V2 39 17, V2 40 17, V2 41 17, V2 42 17, V2 43 17, V2 44 17, V2 45 17, V2 46 17, V2 47 17, V2 48 17, V2 49 17, V2 22 18, V2 23 18, V2 24 18, V2 25 18, V2 23 19, V2 24 19, V2 25 19, V2 14 21, V2 15 21, V2 16 21, V2 17 21, V2 30 20, V2 31 20, V2 32 20, V2 33 20, V2 34 20, V2 45 19, V2 46 19, V2 47 19, V2 48 19, V2 49 19, V2 41 24, V2 42 24, V2 43 24, V2 38 26, V2 39 26, V2 40 26, V2 42 30, V2 43 30, V2 44 30, V2 45 30, V2 46 30, V2 47 30, V2 38 35, V2 39 35, V2 40 35, V2 45 38, V2 46 38, V2 47 38, V2 38 42, V2 39 42, V2 40 42, V2 4 42, V2 5 42, V2 6 42, V2 7 42, V2 8 42, V2 9 42, V2 10 42, V2 11 42, V2 12 42, V2 13 42, V2 14 42, V2 15 42, V2 16 42, V2 17 42, V2 18 42, V2 19 42, V2 20 42, V2 21 42, V2 22 42, V2 23 42, V2 24 42, V2 25 42, V2 26 42, V2 27 42, V2 28 42, V2 29 42, V2 30 42, V2 31 42, V2 32 42, V2 33 42, V2 34 42, V2 35 42, V2 1 38, V2 2 38, V2 3 38, V2 4 38, V2 5 38, V2 6 38, V2 7 38, V2 8 38, V2 8 34, V2 9 34, V2 10 34, V2 11 34, V2 12 34, V2 14 31, V2 15 31, V2 16 31, V2 17 31, V2 18 31, V2 23 28, V2 24 28, V2 25 28, V2 26 28, V2 27 28, V2 28 28, V2 29 28, V2 30 28, V2 34 28, V2 34 33, V2 35 28, V2 35 29, V2 35 30, V2 35 31, V2 35 32, V2 35 33, V2 35 34, V2 35 35, V2 35 36, V2 35 37, V2 35 38, V2 35 39, V2 35 40, V2 35 41, V2 35 42]
-- initPlatform = [V2 2 2, V2 3 3, V2 4 3, V2 5 3, V2 6 3, V2 9 6, V2 10 6, V2 11 6, V2 12 6, V2 16 10, V2 17 10, V2 18 10, V2 43 2, V2 43 1] ++ [ V2 (-1) (-1), V2 (-1) 0, V2 0 (-1), V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0, V2 5 0, V2 6 0, V2 7 0, V2 8 0, V2 9 0, V2 10 0, V2 11 0, V2 12 0, V2 13 0, V2 14 0, V2 15 0, V2 16 0, V2 17 0, V2 18 0, V2 19 0, V2 20 0, V2 21 0, V2 22 0, V2 23 0, V2 24 0, V2 25 0, V2 26 0, V2 27 0, V2 28 0, V2 29 0, V2 30 0, V2 31 0, V2 32 0, V2 33 0, V2 34 0, V2 35 0, V2 36 0, V2 37 0, V2 38 0, V2 39 0, V2 40 0, V2 41 0, V2 42 0, V2 43 0, V2 44 0, V2 45 0, V2 46 0, V2 47 0, V2 48 0, V2 49 0]

defaultGame :: Game
defaultGame  = Game
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
testGameTokEB :: Game
testGameTokEB  = Game
        { _elsa = Character {
            _loc = (V2 0.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = [V2 0 1]
        , _tokensO  = []
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

testGameTokEA :: Game
testGameTokEA  = Game
        { _elsa = Character {
            _loc = (V2 0.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
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

testGameTokOB :: Game
testGameTokOB  = Game
        { _elsa = Character {
            _loc = (V2 0.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = [V2 5 1]
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

testGameTokOA :: Game
testGameTokOA  = Game
        { _elsa = Character {
            _loc = (V2 0.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
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

testGameDeadBefore :: Game
testGameDeadBefore  = Game
        { _elsa = Character {
            _loc = (V2 9.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
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

testGameDeadAfter :: Game
testGameDeadAfter  = Game
        { _elsa = Character {
            _loc = (V2 9.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 5.0 1.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
        , _exits = initExits
        , _platform = initLakesE ++ initLakesO ++ initDeathLakes ++ initPlatform ++ boundaryPlatforms
        , _lakesE = initLakesE ++ initDeathLakes
        , _lakesO = initLakesO ++ initDeathLakes
        , _deathLakes = initDeathLakes
        , _buttons = Map.fromList buttonPlatforms
        , _jump = False
        , _dead   = True
        , _done   = False
        }

testGameDoneBefore :: Game
testGameDoneBefore  = Game
        { _elsa = Character {
            _loc = (V2 34.0 29.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 34.0 34.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
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

testGameDoneAfter :: Game
testGameDoneAfter  = Game
        { _elsa = Character {
            _loc = (V2 34.0 29.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _olaf = Character {
            _loc = (V2 34.0 34.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }
        , _tokensE  = []
        , _tokensO  = []
        , _exits = initExits
        , _platform = initLakesE ++ initLakesO ++ initDeathLakes ++ initPlatform ++ boundaryPlatforms
        , _lakesE = initLakesE ++ initDeathLakes
        , _lakesO = initLakesO ++ initDeathLakes
        , _deathLakes = initDeathLakes
        , _buttons = Map.fromList buttonPlatforms
        , _jump = False
        , _dead   = False
        , _done   = True
        }

testOnPlat :: Character
testOnPlat = Character {
            _loc = (V2 6.0 5.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }

testOffPlat :: Character
testOffPlat = Character {
            _loc = (V2 5.0 5.0)
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
          }