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

pick :: [(Point, Point, Int, [Point])] -> IO (Point, Point, Int, [Point])
pick xs
  | all ((== 0) . \(_,_,y,_)->y) xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)
  | otherwise = do return $ head xs

pickOrder :: [a] -> IO a
pickOrder xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

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
exploreMap2 :: GameState -> Ant -> IO (Maybe(Order, Set Point))
exploreMap2 gs ant =
  case getNeighbors gs (point ant) [] of
    [] -> return (Maybe.Nothing)
    children -> do let dirs = (sortBy sortTup $map (new_bfs gs) children)
                   choice <- pick $ trace ("DIRS ANT"++ (show ant) ++ "\nDIRS "++
                                           show (map (\(x,y,z,_)->(x,y,z)) dirs)) dirs
                   let d1 = directions (world gs) (point ant) $(\(_,x,_,_)->x) (trace ("Choice " ++ show choice) choice)
                       d2 = directions (world gs) (point ant) $(\(x,_,_,_)->x) choice
                       possible_order1 = filter (passable
                                                (world gs)) [Order {ant = ant,
                                                                    direction = fst d1},
                                                             Order {ant = ant,
                                                                    direction = snd d1}]
                       possible_order2 = filter (passable
                                                 (world gs)) [Order {ant = ant,
                                                                     direction = fst d2},
                                                              Order {ant = ant,
                                                                     direction = snd d2}]
                       possible_order3 = filter (passable
                                                 (world gs)) $generateAllOrders ant d2
                     in case possible_order2 of
                     [] -> case possible_order1 of
                       [] -> case possible_order3 of
                         [] -> return Maybe.Nothing
                         orders -> do order <- pickOrder orders
                                      return (Just (order, Set.fromList $(\(_,_,_,z)->z) choice))
                       orders -> do order <- pickOrder orders
                                    return (Just (order, Set.fromList $(\(_,_,_,z)->z) choice))
                     orders -> do order <- pickOrder orders
                                  return (Just (order, Set.fromList $(\(_,_,_,z)->z) choice))

generateAllOrders :: Ant -> (Direction, Direction) -> [Order]
generateAllOrders ant (d1, d2) = map (\x -> Order {ant = ant, direction = x}) dirs where
  dirs = [West, East, South, North] List.\\ [d1, d2]

sortTup (a1, b1, c1, d1) (a2, b2, c2, d2)
  | c1 < c2 = GT
  | c1 > c2 = LT
  | c1 == c2 = EQ

new_bfs :: GameState -> Point -> (Point, Point, Int, [Point])
new_bfs gs p = (p, max_p, orig_val, reached) where
  (orig_val, max_p, _, reached)= bfs2 gs 1 (0, p, 0, [p]) [p]

bfs2 :: GameState -- game state
        -> Int -- number of step
        -> (Int, Point, Int, [Point]) -- tuple passed from
        -> [Point] -- BFS Queue
        -> (Int, Point, Int, [Point])
bfs2 _ _ tup [] = tup
bfs2 gs step tup@(orig_val, max_p, max_val, reached) queue
  -- | (head queue) `elem` (unexplored gs) = (-1, head queue, 0, reached)
  | step <= 10 = foldl' (bfs2 gs (step+1)) new_tup (map (: (tail queue)) children)
  | otherwise =  (e_val (head queue) + orig_val, new_point, new_val, reached) where
    new_tup = (orig_val, new_point, new_val, reached ++ children)
    (new_point, new_val) = if max_val < e_val (head queue)
                           then (head queue, e_val (head queue))
                           else (max_p, max_val)
    children = getNeighbors gs (head queue) reached
    e_val point = exploreValue ((world gs) %! point)
{-# INLINE bfs2 #-}