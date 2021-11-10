{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Types
import MyCharacter

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
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
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
    threadDelay 100000 -- controls how fast game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handles keyboard events
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ step UpDir g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ step DownDir g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ step RightDir g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ step LeftDir g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _                                     = continue g

-- Draws game
drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 10) (drawInfo g) <+> drawGrid g ]

-- Draws instruction/game over section
drawInfo :: Game -> Widget Name
drawInfo g = hLimit 40
  $ vBox [ drawInstrs
         , drawGameOver (g ^. dead)
         , drawPassed (g ^. done)
         ]

-- Manages instructions
drawInstrs :: Widget Name
drawInstrs =
  withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Instructions")
  $ C.hCenter
  $ padAll 1
  $ str "Press arrows to begin \n Press 'q' to quit \n Press 'r' to restart"

-- Manages "Level Failed" message
drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "LEVEL FAILED"
     else emptyWidget

-- Manages "Level Passed" message
drawPassed :: Bool -> Widget Name
drawPassed done =
  if done
     then withAttr exitMsgAttr $ C.hCenter $ str "LEVEL PASSSED!!"
     else emptyWidget

-- Draws game grid and cells
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Elsa and Olaf")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. myCharacter = MyCharacter
      | c `elem` g ^. tokens = Token
      | c `elem` g ^. exits  = Exit
      | otherwise            = Empty

-- Renders cell based on type
drawCell :: Cell -> Widget Name
drawCell MyCharacter = withAttr myCharacterAttr cellWidth
drawCell Token  = withAttr tokenAttr cellWidth
drawCell Exit  = withAttr exitAttr cellWidth
drawCell Empty = withAttr emptyAttr cellWidth

cellWidth :: Widget Name
cellWidth = str "  "

-- Maps cell type to appearance
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (myCharacterAttr, V.blue `on` V.blue)
  , (tokenAttr, V.red `on` V.red)
  , (exitAttr, V.green `on` V.green)
  , (exitMsgAttr, fg V.green `V.withStyle` V.bold)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

myCharacterAttr, tokenAttr, exitAttr, exitMsgAttr, emptyAttr, gameOverAttr :: AttrName
myCharacterAttr = "myCharacterAttr"
tokenAttr = "tokenAttr"
exitAttr = "exitAttr"
exitMsgAttr = "exitMsgAttr"
emptyAttr = "emptyAttr"
gameOverAttr = "gameOver"
