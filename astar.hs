module AStar where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.PSQueue as PSQueue
import Data.PSQueue (PSQ, findMin, deleteMin, Binding)
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.List (foldr)
import Ants hiding (Nothing)
import Data.Maybe (fromMaybe)

data AStar a n = AStar { closedSet  :: !(Set Point),
                         openSetPQ :: !(PSQ Point Int),
                         gScore     :: !(Map Point Int),
                         cameFrom   :: !(Map Point Point),
                         end         :: !(Maybe Point) }

initAStar start = AStar { closedSet  = Set.singleton start,
                          openSetPQ = PSQueue.singleton start 0,
                          gScore     = Map.singleton start 0,
                          cameFrom   = Map.empty,
                          end         = Nothing }

-- A* Algorithm implementation
-- Referenced from http://en.wikipedia.org/wiki/A*_search_algorithm
-- recursively calls
aStar ::
         World ->
         Point ->             -- start vertex
         (Point -> [Point])     -- function that maps a node to its neighbors.
         -> (Point -> Bool)   -- The goal. boolean condition on the end vertex.
         -> (Point -> Point -> Int) -- Distance function between vertices of the graph.
         -> (Point -> Int)      -- Heuristic function. Should never overestimate the actual cost to make the solution optimal.
         -> Maybe [Point] -- next node to
aStar world start neighbor isGoal cost heur =
  let s = astar' $ initAStar start where
        astar' star
          | PSQueue.null $ openSetPQ star = star
          | isGoal node = star {end = Just node}
          | otherwise = astar' $ foldl (updateOrAddChild node)
                        star { openSetPQ = deleteMin (openSetPQ star),
                               closedSet = Set.insert node $ closedSet star } neighbors
          where
            -- pick the node in open_set_p having the lowest f_val
            (node, f_val) = case PSQueue.findMin $ openSetPQ star of
              Nothing -> error "no find min in non-empty PSQueue"
              Just (k PSQueue.:-> p) -> (k, p)
            -- use only neighbors not in closedSet
            neighbors = filter (`Set.notMember` (closedSet star)) $ getNeighbors world node

        updateOrAddChild node star' child =
          let newCost = ((gScore star') ! node) + (cost node child)
              star'' = star'{cameFrom = Map.insert child node (cameFrom star')}
          in case PSQueue.lookup child (openSetPQ star') of
            Nothing -> addChild node child newCost star''
            Just _ -> if newCost < gScore star' ! child
                      then updateChild node child newCost star'
                      else star''
        addChild node child newCost star' =
          star' {gScore = Map.insert child newCost (gScore star'),
                 openSetPQ = PSQueue.insert child (newCost + heur child)
                               (openSetPQ star')}
        updateChild node child newCost star' =
          star' {gScore = Map.adjust (\_ -> newCost) child (gScore star'),
                 openSetPQ = PSQueue.adjust (\_ -> newCost + heur child)
                               child (openSetPQ star')}
  in case end s of
    Nothing    -> Nothing
    Just goal  -> Just ((takeWhile (not . (== start)) . reverse .
                               iterate (cameFrom s !) $ goal))

getNeighbors :: World -> Point -> [Point]
getNeighbors world p =
  [n | n <- [nc, sc, wc, ec], tile (world %! n) `notElem` [Water, MyHill]] where
    nc = move North p
    wc = move West p
    sc = move South p
    ec = move East p

-- A* using manhattan distance as heuristic
findAStar :: World -> Point -> Point -> [Point]
findAStar world source dest =
  fromMaybe [] (aStar world source (getNeighbors world) (== dest)
                (\x y -> 1) (manhattan (rowBound world, colBound world) source))