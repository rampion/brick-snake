{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module UI where

import Control.Monad (forever, void)
-- import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
-- import Data.Maybe (fromMaybe)
import System.Random (newStdGen)

import Snake

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padTop, {- padLeft, -} padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Widgets.Center as Center
import qualified Graphics.Vty as Vty
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be continuously fed into the app
data Tick = Tick

-- | Named Resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

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
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast you game moves
  g <- initGame <$> newStdGen
  void $ customMain (Vty.mkVty Vty.defaultConfig) (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g = \case
  AppEvent Tick -> if g ^. dead 
                    then halt g
                    else continue $ step g
  VtyEvent (Vty.EvKey k []) -> case k of
    Vty.KUp       -> continue $ turn North g
    Vty.KDown     -> continue $ turn South g
    Vty.KRight    -> continue $ turn East g
    Vty.KLeft     -> continue $ turn West g
    Vty.KEsc      -> halt g

    Vty.KChar 'k' -> continue $ turn North g
    Vty.KChar 'j' -> continue $ turn South g
    Vty.KChar 'l' -> continue $ turn East g
    Vty.KChar 'h' -> continue $ turn West g
    Vty.KChar 'q' -> halt g

    _ -> continue g
  _ -> continue g
    
    

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = return .  Center.center $ 
  padRight (Pad 2) (drawStats g) <+> drawGrid g

drawStats :: Game -> Widget Name
drawStats g = hLimit 11 $ vBox
  [ drawScore (g ^. score)
  , padTop (Pad 2) $ drawGameOver (g ^. dead)
  ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle Border.unicodeBold
  . vLimit 5
  . Border.borderWithLabel (str "Score")
  . Center.center
  . padAll 1
  . str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead
  | dead      = withAttr gameOverAttr
              . Center.hCenter
              $ str "GAME OVER"
  | otherwise = emptyWidget

gameOverAttr, snakeAttr, foodAttr, emptyAttr :: AttrName
gameOverAttr  = "gameOver"
snakeAttr     = "snakeAttr"
foodAttr      = "foodAttr"
emptyAttr     = "emptyAttr"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle Border.unicodeBold
  . Border.borderWithLabel (str "Snake")
  . vBox $ do
      y <- [height-1,height-2..0]
      return . hBox $ do
        x <- [0..width-1]
        let c = V2 x y
            a | c `elem` snake g  = snakeAttr
              | c == g ^. food    = foodAttr
              | otherwise         = emptyAttr
        return . withAttr a $ str "  "

theMap :: AttrMap
theMap = attrMap Vty.defAttr
  [ (snakeAttr,     Vty.blue `on` Vty.blue)
  , (foodAttr,      Vty.red `on` Vty.red)
  , (gameOverAttr,  fg Vty.red `Vty.withStyle` Vty.bold)
  ]

