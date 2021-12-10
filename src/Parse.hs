-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE ConstraintKinds           #-}
module Parse where

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)
import Types
import Env

-- type MonadGame m = (MonadState BuildingGameState m)

-- readCoords :: (MonadGame m) => Cell -> m [GridCoord]
-- readCoords x = do 
--   GS _ s <- get 
--   return (Map.findWithDefault [] x s)

-- appendCoords :: (MonadGame m) => Cell -> GridCoord -> m ()
-- appendCoords x v = do 
--   GS c s <- get
--   let s' = Map.insertWith (++) x [v] s
--   put (GS c s')

nextCell :: GridCoord -> GridCoord
nextCell (V2 x y) = do
    let new_y = if x >= width - 1 then y - 1 else y
    let new_x = if x >= width - 1 then 0 else x + 1
    (V2 new_x new_y)

-- gameP :: BuildingGameState -> Parser BuildingGameState
-- gameP g@GS{curCoord = c@(V2 x y), gStore = s} = do
--     if x >= width - 1 && y <= 0 then 
--         return g
--     else do
--         g' <- try(emptyP g) <|> try(elsaP g) <|> try(olafP g) <|> try(platformP g) <|> try(exitP g)
--         gameP g'

-- skipSpaces :: Parser ()
-- skipSpaces = skipMany space

-- emptyP :: BuildingGameState -> Parser BuildingGameState
-- emptyP g@GS{curCoord = c, gStore = s} = do
--     skipSpaces
--     string "-"
--     skipSpaces
--     return GS{curCoord = nextCell c, gStore = s}

-- elsaP :: BuildingGameState -> Parser BuildingGameState
-- elsaP g@GS{curCoord = c, gStore = s} = do
--     skipSpaces
--     string "EC"
--     skipSpaces
--     let s' = Map.insert Elsa [c] s
--     return GS{curCoord = nextCell c, gStore = s'}

-- olafP :: BuildingGameState -> Parser BuildingGameState
-- olafP g@GS{curCoord = c, gStore = s} = do
--     skipSpaces
--     string "OC"
--     skipSpaces
--     let s' = Map.insert Olaf [c] s
--     return GS{curCoord = nextCell c, gStore = s'}

-- exitP :: BuildingGameState -> Parser BuildingGameState
-- exitP g@GS{curCoord = c, gStore = s} = do
--     skipSpaces
--     string "EX"
--     skipSpaces
--     let s' = Map.insertWith (++) Exit [c] s
--     return GS{curCoord = nextCell c, gStore = s'}

-- platformP :: BuildingGameState -> Parser BuildingGameState
-- platformP g@GS{curCoord = c, gStore = s} = do
--     skipSpaces
--     string "PL"
--     skipSpaces
--     let s' = Map.insertWith (++) Platform [c] s
--     return GS{curCoord = nextCell c, gStore = s'}

gameP :: GridCoord -> Game -> Parser Game
gameP c@(V2 x y) g = do
    if x >= width - 1 && y <= 0 then 
        return g
    else do
        g' <- try(emptyP c g) 
          <|> try(elsaP c g) 
          <|> try(olafP c g) 
          <|> try(platformP c g) 
          <|> try(exitP c g)
          <|> try(eLakeP c g)
          <|> try(oLakeP c g)
          <|> try(dLakeP c g)
        gameP (nextCell c) g'

skipSpaces :: Parser ()
skipSpaces = skipMany space

emptyP :: GridCoord -> Game -> Parser Game
emptyP c@(V2 x y) g = do
    skipSpaces
    string "-"
    skipSpaces
    return g

elsaP :: GridCoord -> Game -> Parser Game
elsaP c@(V2 x y) g = do
    skipSpaces
    string "EC"
    skipSpaces
    let g' = g & elsa .~ (g & _elsa & loc .~ toPreciseCoord c)
    return g'

olafP :: GridCoord -> Game -> Parser Game
olafP c@(V2 x y) g = do
    skipSpaces
    string "OC"
    skipSpaces
    let g' = g & olaf .~ (g & _olaf & loc .~ toPreciseCoord c)
    return g'

exitP :: GridCoord -> Game -> Parser Game
exitP c@(V2 x y) g = do
    skipSpaces
    string "EX"
    skipSpaces
    let g' = g & exits .~ (g ^. exits ++ [c])
    return g'

platformP :: GridCoord -> Game -> Parser Game
platformP c@(V2 x y) g = do
    skipSpaces
    string "PL"
    skipSpaces
    let g' = g & platform .~ (g ^. platform ++ [c])
    return g'

eLakeP :: GridCoord -> Game -> Parser Game
eLakeP c@(V2 x y) g = do
    skipSpaces
    string "EL"
    skipSpaces
    let g' = (g & lakesE .~ (g ^. lakesE ++ [c]))
    let g'' = (g' & platform .~ (g' ^. platform ++ [c]))
    return g''

oLakeP :: GridCoord -> Game -> Parser Game
oLakeP c@(V2 x y) g = do
    skipSpaces
    string "OL"
    skipSpaces
    let g' = (g & lakesO .~ (g ^. lakesO ++ [c]))
    let g'' = (g' & platform .~ (g' ^. platform ++ [c]))
    return g''
    
dLakeP :: GridCoord -> Game -> Parser Game
dLakeP c@(V2 x y) g = do
    skipSpaces
    string "DL"
    skipSpaces
    let g1 = (g & deathLakes .~ (g ^. deathLakes ++ [c]))
    let g2 = (g1 & lakesO .~ (g1 ^. lakesO ++ [c]))
    let g3 = (g2 & lakesE .~ (g2 ^. lakesE ++ [c]))
    let g4 = (g3 & platform .~ (g3 ^. platform ++ [c]))
    return g4

-- parseFile :: FilePath -> IO (Either ParseError BuildingGameState)
-- parseFile f = do
--     let g = GS {
--         curCoord = (V2 0 (height - 1)),
--         gStore = Map.empty
--     }
--     parseFromFile (gameP g) f

parseFile :: FilePath -> IO (Either ParseError Game)
parseFile f = do
    let g = Game { 
        _elsa = Character {
            _loc = (V2 (-1.0) (-1.0))
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
            }
        , _olaf = Character {
            _loc = (V2 (-1.0) (-1.0))
            , _hv = div maxSpeed 2
            , _vv = div maxSpeed 2
            }
        , _tokensE  = []
        , _tokensO  = []
        , _exits = []
        , _platform = boundaryPlatforms
        , _lakesE = []
        , _lakesO = []
        , _deathLakes = []
        , _buttons = Map.empty
        , _jump   = False
        , _dead   = False
        , _done   = False
        }
    parseFromFile (gameP (V2 0 (height - 1)) g) f

-- readCoords :: Cell -> Store -> Bool -> [GridCoord]
-- readCoords x s required = do 
--   case Map.lookup x s of
--     Just v  -> v
--     Nothing -> if required then error ("Missing Required Values for " ++ show x) else []


-- buildGame :: BuildingGameState -> Game
-- buildGame GS{curCoord = c, gStore = s} = do
--     let elsa_loc = toPreciseCoord (head (readCoords Elsa s True))
--     let olaf_loc = toPreciseCoord (head (readCoords Olaf s True))

--     let tok_e = readCoords TokenE s False
--     let tok_o = readCoords TokenO s False

--     let exits_loc = readCoords Exit s True
--     let platforms_loc = readCoords Platform s False

--     let lakes_e = readCoords LakeE s False
--     let lakes_o = readCoords LakeO s False
--     let lakes_d = readCoords DeathLake s False

--     Game { 
--         _elsa = Character {
--             _loc = elsa_loc
--             , _hv = div maxSpeed 2
--             , _vv = div maxSpeed 2
--             }
--         , _olaf = Character {
--             _loc = olaf_loc
--             , _hv = div maxSpeed 2
--             , _vv = div maxSpeed 2
--             }
--         , _tokensE  = tok_e
--         , _tokensO  = tok_o
--         , _exits = exits_loc
--         , _platform = lakes_e ++ lakes_o ++ lakes_d ++ platforms_loc ++ boundaryPlatforms
--         , _lakesE = lakes_e ++ lakes_d
--         , _lakesO = lakes_o ++ lakes_d
--         , _deathLakes = lakes_d
--         , _buttons = Map.empty
--         , _jump   = False
--         , _dead   = False
--         , _done   = False
--         }