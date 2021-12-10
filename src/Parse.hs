module Parse where

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)
import Types
import Env

nextCell :: GridCoord -> GridCoord
nextCell (V2 x y) = do
    let new_y = if x >= width - 1 then y - 1 else y
    let new_x = if x >= width - 1 then 0 else x + 1
    (V2 new_x new_y)

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
          <|> try(eTokenP c g)
          <|> try(oTokenP c g)
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

eTokenP :: GridCoord -> Game -> Parser Game
eTokenP c@(V2 x y) g = do
    skipSpaces
    string "ET"
    skipSpaces
    let g' = (g & tokensE .~ (g ^. tokensE ++ [c]))
    return g'

oTokenP :: GridCoord -> Game -> Parser Game
oTokenP c@(V2 x y) g = do
    skipSpaces
    string "OT"
    skipSpaces
    let g' = (g & tokensO .~ (g ^. tokensO ++ [c]))
    return g'

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
