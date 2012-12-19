module BFS where
import Ants
-- Ants also has type Nothing
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Prelude

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
  [child | child <- [nc, wc, sc, ec], tile ((world gs) %! child) `notElem` [Water, MyHill],
   child `notElem` reached] where
      nc = move North p
      wc = move West p
      sc = move South p
      ec = move East p

--perform BFS to explore Tiles
exploreMap :: GameState -> Ant -> Maybe (Order, Set Point)
exploreMap gs ant
  | Maybe.isNothing dest = Maybe.Nothing
  | otherwise = case possible_dir of Maybe.Nothing -> Maybe.Nothing
                                     Just dir -> Just (dir, Set.empty)
  where
    dest = bfs gs [point ant] [point ant]
    dir = directions (world gs) (point ant) (Maybe.fromMaybe (0, 0) dest)
    possible_dir = List.find (passable (world gs)) [Order {ant = ant, direction = fst dir},
                                           Order {ant = ant, direction = snd dir}]

-- new exploration method!

-- -- Adds root point to the queue and initiates BFS
-- exploreMap :: GameState -> Ant -> Maybe (Order, Set Point)
-- exploreMap gs ant = case getNeighbors gs (point ant) [] of
--   [] -> Maybe.Nothing
--   children -> let dir = dfs gs 0 [] (point ant) (Set.fromList children)
--                   d = directions (world gs) (point ant) (head . sort) $ map fst dir
--                   in
--                Just (find (passable (world gs)) [Order {ant = ant, direction = fst d},
--                                                  Order {ant = ant, direction = snd d}],
--                     Set.unions (map snd dir))

-- --
-- dfs :: GameState -> Int -> [Point] -> Set Point -> Point -> (Int, Set Point)
-- dfs gs step reached point initSet =
--   let children = (getNeighbors gs point reached step) in
--   if step <= 10 then foldr (dfs gs (step+1) (reached ++ children) (Set.union initSet updateSet)) 0 children
--   else exploreValue $ (gs world) %! point
