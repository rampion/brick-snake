{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Snake where

import Prelude hiding (head)
import Control.Category ((>>>))

import Control.Monad.State.Strict (runState, state)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewR(..), (<|))
import qualified Data.Sequence as Seq
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), StdGen)

-- Types

data Game = Game
  { _head   :: Coord        -- ^ head of the snake
  , _body   :: Seq Coord    -- ^ body of the snake as a sequence of points in R2
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of food
  , _seed   :: StdGen       -- ^ generator for next food location
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _frozen :: Bool         -- ^ freeze to disallow duplicate turns
  }

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West

data Axis
  = Vertical
  | Horizontal
  deriving Eq

$(makeLenses ''Game)

-- Constants

height, width :: Int
height = 20
width  = 20

-- Functions

-- | Step forward in time
step :: Game -> Game
step g | g ^. paused || g ^. dead = g
       | otherwise                = die g' `orElse` eatFood g' `orElse` move g'
  where g' = g & frozen .~ False

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
infixr 1 `orElse`

-- | Possibly die if next head position is disallowed
die :: Game -> Maybe Game
die g | nh g `elem` snake g = Just $ g & dead .~ True
      | otherwise           = Nothing

-- | Get the entirety of the game's snake
snake :: Game -> Seq Coord
snake g = g ^. head <| g ^. body

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe Game
eatFood g | nh g == g ^. food = Just $ g & body .~ snake g
                                         & head .~ nh g
                                         & score %~ (+10)
                                         & nextFood
          | otherwise         = Nothing

-- | Set a valid food coordinate
nextFood :: Game -> Game
nextFood g | f `elem` snake g = nextFood g'
           | otherwise        = g' & food .~ f
  where (f, g') = g & seed randomCoord

randomCoord :: StdGen -> (Coord, StdGen)
randomCoord = runState $ do
  x <- state $ randomR (0, width - 1)
  y <- state $ randomR (0, height - 1)
  return $ V2 x y

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = g & body .~ g ^. head <| init (g ^. body)
           & head .~ nh g
  where init = Seq.viewr >>> \case
          EmptyR  -> Seq.Empty
          s :> _  -> s

-- | Get the next head location of the game's snake
nh :: Game -> Coord
nh g = nextHead (g ^. head) (g ^. dir)

-- | Get the next head position of a snake in a particular direction
nextHead :: Coord -> Direction -> Coord
nextHead a = \case
  North -> a & _y %~ \y -> (y + 1) `mod` height
  South -> a & _y %~ \y -> (y - 1) `mod` height
  East  -> a & _x %~ \x -> (x + 1) `mod` width
  West  -> a & _x %~ \x -> (x - 1) `mod` width

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g | g ^. frozen = g
         | otherwise   = g & dir %~ turnDir d
                           & paused .~ False
                           & frozen .~ True

turnDir :: Direction -> Direction -> Direction
turnDir new curr
  | axis new /= axis curr = new
  | otherwise             = curr

axis :: Direction -> Axis
axis North  = Vertical
axis South  = Vertical
axis East   = Horizontal
axis West   = Horizontal

-- | Initialize a paused game
initGame :: StdGen -> Game
initGame stdGen = nextFood Game
  { _head   = V2 (width `div` 2) (height `div` 2)
  , _body   = Seq.empty
  , _dir    = North
  , _food   = error "will be initialized by nextFood"
  , _seed   = stdGen
  , _dead   = False
  , _paused = True
  , _score  = 0
  , _frozen = False
  }
