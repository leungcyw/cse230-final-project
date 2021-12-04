
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude
import Env as E
import Types as T
import Constants as C
import UI hiding (main)
import Linear.V2 (V2(..), _x, _y)

main :: IO ()
main = runTests 
  [ 
      testLoc
    , testColl
    , testToks
    , testEnd
    , testButtons
    , moveChars
  ]

scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String, Score) -> TestTree
scoreTest (f, x, r, n, msg, sc) = scoreTest' sc (return . f, x, r, n, msg)

testLoc ::  Score -> TestTree
testLoc sc = testGroup "Character Location" [
    scoreTest ((\_ -> E.getCoord C.defaultGame 'e'), (), (V2 0.0 1.0), 10, "getCoord-E", sc)
  , scoreTest ((\_ -> E.getCoord C.defaultGame 'o'), (), (V2 1.0 1.0), 10, "getCoord-O", sc)
  ]

testColl ::  Score -> TestTree
testColl sc = testGroup "Platforms" [
    scoreTest ((\_ -> E.collisionH (V2 1 1) C.defaultGame RightDir 1), (), False, 10, "checkColl-HF", sc)
  , scoreTest ((\_ -> E.collisionV (V2 0 0) C.defaultGame), (), False, 10, "checkColl-VF", sc)
  , scoreTest ((\_ -> E.collisionH (V2 47 1) C.defaultGame RightDir 1), (), True, 10, "checkColl-HT", sc)
  , scoreTest ((\_ -> E.collisionV (V2 6 3) C.defaultGame), (), True, 10, "checkColl-VT", sc)
  , scoreTest ((\_ -> E.onPlatform C.testOnPlat C.defaultGame), (), 5, 10, "checkPlatform-T", sc)
  , scoreTest ((\_ -> E.onPlatform C.testOffPlat C.defaultGame), (), -1, 10, "checkPlatform-T", sc)
  ]

testToks ::  Score -> TestTree
testToks sc = testGroup "Tokens" [
    scoreTest ((\_ -> E.eatTokenE C.defaultGame), (), C.defaultGame, 10, "eatToken-EF", sc)
  , scoreTest ((\_ -> E.eatTokenE C.testGameTokEB), (), C.testGameTokEA, 10, "eatToken-ET", sc)
  , scoreTest ((\_ -> E.eatTokenO C.defaultGame), (), C.defaultGame, 10, "eatToken-OF", sc)
  , scoreTest ((\_ -> E.eatTokenO C.testGameTokOB), (), C.testGameTokOA, 10, "eatToken-OT", sc)
  ]

testEnd :: Score -> TestTree
testEnd sc = testGroup "End Conditions" [
    scoreTest ((\_ -> E.die C.defaultGame), (), C.defaultGame, 10, "dead-False-OutLakes", sc)
  , scoreTest ((\_ -> E.die C.testGameDeadBefore4), (), C.testGameDeadBefore4, 10, "dead-False-InLakes", sc)
  , scoreTest ((\_ -> E.die C.testGameDeadBefore), (), C.testGameDeadAfter, 10, "dead-True-Elsa", sc)
  , scoreTest ((\_ -> E.die C.testGameDeadBefore2), (), C.testGameDeadAfter2, 10, "dead-True-Olaf", sc)
  , scoreTest ((\_ -> E.die C.testGameDeadBefore3), (), C.testGameDeadAfter3, 10, "dead-True-PoisonLake", sc)
  , scoreTest ((\_ -> E.checkDone C.defaultGame), (), C.defaultGame, 10, "done-False", sc)
  , scoreTest ((\_ -> E.checkDone C.testGameDoneBefore), (), C.testGameDoneAfter, 10, "done-True", sc)
  ]

testButtons :: Score -> TestTree
testButtons sc = testGroup "Buttons/Platforms" [
    scoreTest ((\_ -> E.getAllGridCoordsForButtonPlatform (V2 37 19)), (), [V2 37 19, V2 38 19, V2 39 19], 10, "curr-platform", sc)
  , scoreTest ((\_ -> E.getButtonPlatformLocs C.buttonGame), (), [V2 45 19, V2 46 19, V2 47 19, V2 37 19, V2 38 19, V2 39 19], 10, "all-platforms", sc)
  , scoreTest ((\_ -> E.platformDirection (V2 1 0) (V2 1 2)), (), 1, 10, "platform-dir", sc)
  , scoreTest ((\_ -> E.moveButtonPlatform C.buttonGame), (), C.buttonGameAfter, 10, "button-press", sc)
  ]

moveChars :: Score -> TestTree
moveChars sc = testGroup "Move Characters" [
    scoreTest ((\_ -> E.move 'e' RightDir False C.defaultGame), (), C.defaultGameAfterMoveR, 10, "move-right", sc)
  , scoreTest ((\_ -> E.move 'o' LeftDir False C.defaultGame), (), C.defaultGameAfterMoveL, 10, "move-left", sc)
  , scoreTest ((\_ -> E.move 'e' DownDir False C.defaultGame), (), C.defaultGameAfterMoveD, 10, "move-down", sc)
  , scoreTest ((\_ -> E.move 'o' Neutral True C.defaultGame), (), C.defaultGameAfterMoveU, 10, "move-up", sc)
  ]
