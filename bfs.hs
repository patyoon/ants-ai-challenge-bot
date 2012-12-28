module BFS where
import Ants
-- Ants also has type Nothing
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set, union, fromList, unions)
import Data.List (foldl', find, sortBy)
import qualified Data.Set as Set
import qualified Data.List as List
import Prelude
import Debug.Trace (trace)
import System.IO
import System.Random (randomRIO)
import Control.Monad

pick :: [(Point, Int)] -> IO (Point, Int)
pick xs
  | all ((== 0) . snd) xs = do return $ head xs
  | otherwise = randomRIO (0, (length xs - 1)) >>= return . (xs !!)


--Recursive function that performs the breadth first search
bfs :: GameState ->
       [Point] -> -- BFS FIFO Queue
       [Point] -> -- list of already reached nodes. Do not pick them as neighbors
       Maybe Point -- return first unexplored point
-- this case should never happen
bfs gs [] reached = Maybe.Nothing
bfs gs queue reached = if (head queue) `elem` (unexplored gs)
                       then Maybe.Just $ head queue
                       else bfs gs ((tail queue) ++ neighbors) (reached ++ neighbors) where
                         neighbors = (getNeighbors gs (head queue) reached)

-- Returns children of the point that are passable and havent been reached
getNeighbors :: GameState -> Point -> [Point] -> [Point]
getNeighbors gs p reached =
  [child | child <- [nc, wc, sc, ec], tile ((world gs) %! child)
                                      `notElem` [Water, MyHill],
   child `notElem` reached] where
      nc = moveW (world gs) North p
      wc = moveW (world gs) West p
      sc = moveW (world gs) South p
      ec = moveW (world gs) East p

--perform BFS to explore Tiles
exploreMap :: GameState -> Ant -> Maybe (Order, Set Point)
exploreMap gs ant
  = let dest =  bfs gs [point ant] [point ant]
        dir = directions (world gs) (point ant) (Maybe.fromMaybe (0, 0) dest)
        possible_order = List.find (passable (world gs)) [Order {ant = ant, direction = fst dir},
                                                          Order {ant = ant, direction = snd dir}] in
    if Maybe.isNothing dest then Just (Order {ant = ant, direction = Ants.North}, Set.empty)
    else case possible_order of
      Maybe.Nothing -> Maybe.Nothing
      Just order -> Just (order, Set.empty) where

-- new exploration method!

-- Adds root point to the queue and initiates BFS
exploreMap2 :: GameState -> Ant -> IO (Maybe Order)
exploreMap2 gs ant =
  case getNeighbors gs (point ant) [] of
    [] -> return (Maybe.Nothing)
    children -> do choice <- pick (sortBy sortTup $map (new_bfs gs) children)
                   let d = directions (world gs) (point ant) $fst choice
                       possible_order = find (passable
                                           (world gs)) [Order {ant = ant,
                                                               direction = fst d},
                                                        Order {ant = ant,
                                                               direction = snd d}]
                     in case possible_order of
                     Maybe.Nothing -> return (Maybe.Nothing)
                     Just order -> return (Just order)

sortTup (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = EQ

new_bfs :: GameState -> Point -> (Point, Int)
new_bfs gs p = (p, val) where
  val = bfs2 gs 1 [p] 0 [p]

bfs2 :: GameState -- game state
        -> Int -- number of step
        -> [Point] -- Visited node queue
        -> Int -- tuple passed from
        -> [Point] -- BFS Queue
        -> Int
bfs2 _ _ _ val [] = val
bfs2 gs step reached val queue
  -- | (head queue) `elem` (unexplored gs) = val
  | step <= 10 = foldl' (bfs2 gs (step+1) (reached ++ children)) val (map (: (tail queue)) children)
  | otherwise =  e_val (head queue) + val where
    children = getNeighbors gs (head queue) reached
    e_val point = exploreValue ((world gs) %! point)
{-# INLINE bfs2 #-}