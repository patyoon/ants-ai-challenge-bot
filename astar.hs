module AStar where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.PSQueue as PSQueue
import Data.PSQueue (PSQ, findMin, deleteMin, Binding(..), minView)
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.List (foldl')
import Ants hiding (Nothing)
import Data.Maybe (fromMaybe)

data AStar a n = AStar { closedSet  :: !(Set Point),
                         openSetPQ :: !(PSQ Point Int),
                         gScore     :: !(Map Point Int),
                         cameFrom   :: !(Map Point Point),
                         heurMap    :: !(Map Point Int),
                         end         :: !(Maybe Point) }
                 deriving (Show)

initAStar start = AStar { closedSet = Set.singleton start,
                          openSetPQ = PSQueue.singleton start 0,
                          gScore    = Map.singleton start 0,
                          cameFrom  = Map.empty,
                          heurMap   = Map.empty,
                          end       = Nothing }

-- A* Algorithm implementation
-- Referenced from http://en.wikipedia.org/wiki/A*_search_algorithm
-- recursively calls
aStar ::
         Point ->             -- start vertex
         (Point -> [Point])     -- function that maps a node to its neighbors.
         -> (Point -> Bool)   -- The goal. boolean condition on the end vertex.
         -> (Point -> Point -> Int) -- Distance function between vertices of the graph.
         -> (Point -> Int)      -- Heuristic function. Should never overestimate the actual cost to make the solution optimal.
         -> Maybe [Point] -- next node to
aStar start neighbor isGoal cost heur
  = let res = aStar' (initAStar start)
    in case end res of
      Nothing -> Nothing
      Just goal -> Just (reverse . takeWhile (not. (==start)) .
                         iterate (cameFrom res !) $ goal)
  where aStar' star
          = case minView $ openSetPQ star of
              Nothing -> star
              Just (node :-> _, pq') ->
                if isGoal node
                  then star { end = Just node}
                  else aStar' $ foldl' (expand node)
                                       (star { openSetPQ = pq',
                                               closedSet = Set.insert node (closedSet star)})
                       -- use only neighbors not in closedSet
                       (filter (`Set.notMember` (closedSet star)) $ neighbor node)
        expand node star child
          = let newCost = gScore star ! node + cost node child
            in case PSQueue.lookup child (openSetPQ star) of
                 Nothing -> updateChild node child newCost
                              (star { heurMap
                                     = Map.insert child (heur child) (heurMap star) })
                 Just _  -> if newCost < gScore star ! child
                              then updateChild node child newCost star
                              else star
        updateChild node child cost star
           = star { cameFrom = Map.insert child node (cameFrom star),
                    gScore    = Map.insert child cost (gScore star),
                    openSetPQ  = PSQueue.insert child (cost + heurMap star ! child) (openSetPQ star) }
{-# INLINE aStar #-}

getNeighbors :: World -> Point -> [Point]
getNeighbors world p =
  [n | n <- [nc, sc, wc, ec], tile (world %! n) `notElem` [MyAnt, Water, MyHill]] where
    nc = move North p
    wc = move West p
    sc = move South p
    ec = move East p
{-# INLINE getNeighbors #-}

-- A* using manhattan distance as heuristic
findAStar :: World -> Point -> Point -> [Point]
findAStar world source dest =
  fromMaybe [] (aStar source (getNeighbors world) (== dest)
                (\x y -> 1) (manhattan (rowBound world, colBound world) source))