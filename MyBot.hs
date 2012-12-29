module Main where
import Data.List
import Data.Maybe (mapMaybe, catMaybes)
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import BFS (exploreMap, exploreMap2)
import Ants
import Data.Set (Set)
import AStar (findAStar)
import Data.Ord

-- import AlphaBeta (runAlphaBeta)

-- Picks the first passable order in a list
-- and returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- creates orders. Called by updateTurn
doTurn :: GameParams -> GameState -> IO ([Order], Set Point)
doTurn gp gs = do
  --Food gathering.
  -- foodOrders :: [(path lenturnh, (Food, Maybe Order))]
  -- create for all possible food, ant pair
    -- check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  maybeExploreOrders <- mapM (exploreMap2 gs) freeAnts
  let exploreOrders = catMaybes maybeExploreOrders
      expTurn = updateTurn (world gs)  (foodTurn)
                    (map fst exploreOrders)
      -- (Turn {pointOrders = Map.empty, foodToAnts = Map.empty}) --
      freeAnts2 = [myant | myant <- myAnts (ants gs),
                   myant `notElem` (map ant (Map.elems (pointOrders expTurn)))]
          --unblock our hills to enable spawning of our ants
      hillOrders = [[Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = North},
                         Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = West},
                         Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = South},
                         Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = East}]
                       | h <- (hills gs),
                         (hillPoint h) `elem` (map point (myAnts (ants gs)))]
      unblockHillsOrders = mapMaybe (tryOrder (world gs)) hillOrders
      hillTurn = updateTurn (world gs) expTurn
                 unblockHillsOrders
      orders = Map.elems $ pointOrders hillTurn
  -- wrap list of orders back into a monad
  return (orders, Set.unions $map snd exploreOrders) where
    foodOrders = [(len, (food_loc, tryOrder (world gs) [Order {ant = myant,
                                                               direction = fst d},
                                                        Order {ant = myant,
                                                               direction = snd d}]))
                 | food_loc <- food gs,
                   let myant = head $sortBy (comparing ((distance gp food_loc)
                                                        . point)) (myAnts (ants gs)),
                   -- performs A* search to food
                   -- let (nextPoint, len) = findAStar (world gs) (point myant) food_loc,
                   let path = findAStar (world gs) (point myant) food_loc,
                   let len = length path,
                   len /= 0,
                   let nextPoint = head path,
                   let d = directions (world gs) (point myant) nextPoint]
    -- sort order based on the length of path
    sortedFoodOrders = map snd (sort  foodOrders)
    -- foodTurn is Turn containing food orders
    foodTurn = updateTurnFood (world gs)
               (Turn {pointOrders = Map.empty, foodToAnts = Map.empty}) sortedFoodOrders
    -- Exploring the map
    -- Get free ants
    freeAnts = [myant | myant <- myAnts (ants gs),
                myant `notElem` (map ant (Map.elems (pointOrders foodTurn)))]
    --  explore map

-- Recursively update pointOrders of PointOrders type
-- in Turn with orders in the list.
updateTurnFood :: World -> Turn -> [(Food, Maybe Order)] -> Turn
updateTurnFood world turn [] = turn
updateTurnFood world turn foodorders = updateTurnFood world (unClaimedFood turn (head foodorders)) (tail foodorders)

-- Recursively update pointOrders of PointOrders type
-- in Turn with orders in the list. could be written in foldr.
updateTurn :: World -> Turn -> [Order] -> Turn
updateTurn world turn [] = turn
updateTurn world turn orders = updateTurn world (unoccupied turn
                                                 (head orders)) (tail orders)
-- runs the game
main :: IO ()
main = game doTurn
