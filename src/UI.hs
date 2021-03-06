{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Types
import Env
import Parse

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Data.Map as Map
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))

-- App definition
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 10000 -- controls how fast game moves
  g <- initGame defaultGameState
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

customInit1 :: IO Game
customInit1 = do
  g <- parseFile "levels/level1.txt"
  case g of
    Left l -> error ("Error: " ++ (show l))
    Right r -> return r

customInit2 :: IO Game
customInit2 = do
  g <- parseFile "levels/level2.txt"
  case g of
    Left l -> error ("Error: " ++ (show l))
    Right r -> return r

customInit3 :: IO Game
customInit3 = do
  g <- parseFile "levels/level3.txt"
  case g of
    Left l -> error ("Error: " ++ (show l))
    Right r -> return r

-- Handles keyboard events
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ step 'e' Neutral True g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ step 'e' DownDir False g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ step 'e' RightDir False g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ step 'e' LeftDir False g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ step 'o' Neutral True g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ step 'o' DownDir False g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ step 'o' RightDir False g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a')[]))  = continue $ step 'o' LeftDir False g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame defaultGameState) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar '1') [])) = do
  x <- liftIO customInit1
  r <- liftIO (initGame x)
  continue r
handleEvent g (VtyEvent (V.EvKey (V.KChar '2') [])) = do
  x <- liftIO customInit2
  r <- liftIO (initGame x)
  continue r
handleEvent g (VtyEvent (V.EvKey (V.KChar '3') [])) = do
  x <- liftIO customInit3
  r <- liftIO (initGame x)
  continue r
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g

handleEvent g _                                     = continue  $ step 'x' Neutral False g

-- Draws game
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 10) (drawInfo g) <+> drawGrid g ]

-- Draws instruction/game over section
drawInfo :: Game -> Widget Name
drawInfo g = hLimit 50
  $ vBox [ drawInstrs
         , drawGameOver (g ^. dead)
         , drawPassed (g ^. done)
         ]

-- Manages instructions
drawInstrs :: Widget Name
drawInstrs =
  withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Elsa and Olaf")
  $ C.hCenter
  $ padAll 1
  $ str (
         "Objective: Move Olaf and Elsa to collect\n" ++
         "the tokens and reach the green exit doors.\n\n" ++
         "Elsa: Controlled by the arrow keys, collects\n" ++
         "ice cubes, is safe in blue lakes, and dies in\n" ++
         "red and yellow lakes.\n\n" ++
         "Olaf: Conrolled by 'A,' 'W, 'S,' 'D,' collects\n" ++
         "carrots, is safe in red lakes, and dies in\n" ++
         "blue or yellow lakes.\n\n" ++
         "Stand on the buttons ('==') to move the black\n" ++
         "platforms\n\n" ++
         "Press 'q' to quit \n" ++
         "Press 'r' to restart\n\n" ++
         "Press numbers '1,' '2,' and '3,' to load up to\n3 custom levels you can design"
        )

-- Manages "Level Failed" message
drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "LEVEL FAILED D:"
     else emptyWidget

-- Manages "Level Passed" message
drawPassed :: Bool -> Widget Name
drawPassed done =
  if done
     then withAttr exitMsgAttr $ C.hCenter $ str "LEVEL PASSSED :D"
     else emptyWidget

-- Draws game grid and cells
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ vBox rows
  where
    bPlatformData = map (\a -> _platform_loc (snd a)) (Map.toList (g ^. buttons))
    bPlatformExpand = concat $ map getAllGridCoordsForButtonPlatform bPlatformData
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == (toGridCoord (g ^. elsa ^. loc)) = Elsa
      | c == (toGridCoord (g ^. olaf ^. loc)) = Olaf
      | c `elem` g ^. tokensE    = TokenE
      | c `elem` g ^. tokensO    = TokenO
      | c `elem` g ^. deathLakes = DeathLake
      | c `elem` g ^. lakesE     = LakeE
      | c `elem` g ^. lakesO     = LakeO
      | c `elem` g ^. exits      = Exit
      | c `Map.member` (g ^. buttons) = Button
      | c `elem` bPlatformExpand = BPlatform
      | c `elem` g ^. platform   = Platform
      | otherwise                = Empty

-- Renders cell based on type
drawCell :: Cell -> Widget Name
drawCell Elsa = withAttr elsaAttr $ str "????"
drawCell Olaf = withAttr olafAttr $ str "???"
drawCell TokenE = withAttr tokenEAttr $ str "????"
drawCell TokenO = withAttr tokenOAttr $ str "????"
drawCell DeathLake = withAttr deathLakeAttr cellWidth
drawCell LakeE = withAttr lakeEAttr cellWidth
drawCell LakeO = withAttr lakeOAttr cellWidth
drawCell Exit  = withAttr exitAttr cellWidth
drawCell Platform = withAttr platformAttr cellWidth
drawCell Empty = withAttr emptyAttr cellWidth
drawCell Button = withAttr buttonAttr $ str "=="
drawCell BPlatform = withAttr buttonPlatformAttr cellWidth
{-
drawCell Elsa = withAttr elsaAttr cellWidth
drawCell Olaf = withAttr olafAttr cellWidth
drawCell TokenE = withAttr tokenEAttr cellWidth
drawCell TokenO = withAttr tokenOAttr cellWidth 
drawCell DeathLake = withAttr deathLakeAttr cellWidth
drawCell LakeE = withAttr lakeEAttr cellWidth
drawCell LakeO = withAttr lakeOAttr cellWidth
drawCell Exit  = withAttr exitAttr cellWidth
drawCell Platform = withAttr platformAttr cellWidth
drawCell Empty = withAttr emptyAttr cellWidth
drawCell Button = withAttr buttonAttr $ str "=="
drawCell BPlatform = withAttr buttonPlatformAttr cellWidth
-}

cellWidth :: Widget Name
cellWidth = str "  "

-- Maps cell type to appearance
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (elsaAttr, V.brightWhite `on` V.brightWhite)
  , (olafAttr, V.brightWhite `on` V.brightWhite)
  , (tokenEAttr, V.brightWhite `on` V.brightWhite)
  , (tokenOAttr, V.brightWhite `on` V.brightWhite)
  , (deathLakeAttr, V.yellow `on` V.yellow)
  , (lakeEAttr, V.red `on` V.red)
  , (lakeOAttr, V.blue `on` V.blue)
  , (exitAttr, V.brightGreen `on` V.brightGreen)
  , (exitMsgAttr, fg V.green `V.withStyle` V.bold)
  , (emptyAttr, V.brightWhite `on` V.brightWhite)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (platformAttr, V.white `on` V.white)
  , (buttonAttr, V.black `on` V.brightWhite)
  , (buttonPlatformAttr, V.black `on` V.black)
  ]
{-
theMap = attrMap V.defAttr
  [ (elsaAttr, V.blue `on` V.blue) 
  , (olafAttr, V.red `on` V.red)
  , (tokenEAttr, V.cyan `on` V.cyan)
  , (tokenEAttr, V.magenta `on` V.magenta)
  , (deathLakeAttr, V.brightGreen `on` V.brightGreen)
  , (lakeEAttr, V.red `on` V.red)
  , (lakeOAttr, V.blue `on` V.blue)
  , (exitAttr, V.green `on` V.green)
  , (exitMsgAttr, fg V.green `V.withStyle` V.bold)
  , (emptyAttr, V.brightWhite `on` V.brightWhite)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (platformAttr, V.black `on` V.black)
  , (buttonAttr, V.black `on` V.brightWhite)
  , (buttonPlatformAttr, V.brightBlue `on` V.brightBlue)
  ]
-}

elsaAttr, tokenEAttr, tokenOAttr, exitAttr, exitMsgAttr, emptyAttr, gameOverAttr, lakeEAttr, lakeOAttr, olafAttr, platformAttr, deathLakeAttr, buttonAttr, buttonPlatformAttr :: AttrName
elsaAttr = "elsaAttr"
olafAttr = "olafAttr"
tokenEAttr = "tokenEAttr"
tokenOAttr = "tokenOAttr"
exitAttr = "exitAttr"
exitMsgAttr = "exitMsgAttr"
emptyAttr = "emptyAttr"
gameOverAttr = "gameOver"
lakeEAttr = "lakeEAttr"
lakeOAttr = "lakeOAttr"
platformAttr = "platformAttr"
deathLakeAttr = "lakeAttr"
buttonAttr = "buttonAttr"
buttonPlatformAttr = "buttonPlatformAttr"