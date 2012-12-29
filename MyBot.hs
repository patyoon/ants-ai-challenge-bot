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
  elapsedTime <- (timeRemaining gs)
  hPutStrLn stderr $ show elapsedTime
  maybeDiffuseOrders <- mapM (exploreMap2 gs) freeDiffuseAnts
  let diffuseOrders = catMaybes maybeDiffuseOrders
      diffuseTurn = updateTurn (world gs)  unexploredTurn
                    (map fst diffuseOrders)
      -- (Turn {pointOrders = Map.empty, foodToAnts = Map.empty}) --
          --unblock our hills to enable spawning of our ants
      orders = Map.elems $ pointOrders diffuseTurn
  -- wrap list of orders back into a monad
  return (orders, Set.unions $map snd diffuseOrders) where
    unexploredOrder = case freeExploreAnts of
      [] -> []
      xs -> [(len, (closest, tryOrder (world gs) [Order {ant = freeAnt,
                                                                direction = fst d},
                                                         Order {ant = freeAnt,
                                                                direction = snd d}]))
            | freeAnt <- xs,
              let closest = head $sortBy (comparing (distance gp (point freeAnt)))
                            (unexplored gs),
              let path = findAStar (world gs) (point freeAnt) closest,
              let len = length path,
              len /= 0,
              let nextPoint = head path,
              let d = directions (world gs) (point freeAnt) nextPoint]
    -- sort order based on the length of path
    sortedUnexploredOrders = map snd (sort unexploredOrder)
    -- foodTurn is Turn containing food orders
    unexploredTurn = updateTurnFood (world gs)
               hillTurn sortedUnexploredOrders
    hillOrders = [[Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = North},
                   Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = West},
                   Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = South},
                   Order{ant = Ant{point =(hillPoint h), owner = Me}, direction = East}]
                 | h <- (hills gs),
                   (hillPoint h) `elem` (map point (myAnts (ants gs)))]
    unblockHillsOrders = mapMaybe (tryOrder (world gs)) hillOrders
    hillTurn = updateTurn (world gs) foodTurn
               unblockHillsOrders
    foodOrders = [(len, (food_loc, tryOrder (world gs) [Order {ant = closest,
                                                               direction = fst d},
                                                        Order {ant = closest,
                                                               direction = snd d}]))
                 | food_loc <- food gs,
                   let freeAnts = myAnts (ants gs),
                   let closest = head $sortBy (comparing ((distance gp food_loc)
                                                        . point)) freeAnts,
                   -- performs A* search to food
                   -- let (nextPoint, len) = findAStar (world gs) (point myant) food_loc,
                   let path = findAStar (world gs) (point closest) food_loc,
                   let len = length path,
                   len /= 0,
                   let nextPoint = head path,
                   let d = directions (world gs) (point closest) nextPoint]
    -- sort order based on the length of path
    sortedFoodOrders = map snd (sort foodOrders)
    -- foodTurn is Turn containing food orders
    foodTurn = updateTurnFood (world gs)
               (Turn {pointOrders = Map.empty, foodToAnts = Map.empty}) sortedFoodOrders
    -- Exploring the map
    -- Get free ants
    freeExploreAnts = [myant | myant <- myAnts (ants gs),
                       myant `notElem` (map ant (Map.elems (pointOrders foodTurn)))]
    freeDiffuseAnts = [myant | myant <- myAnts (ants gs),
                 myant `notElem` (map ant (Map.elems (pointOrders unexploredTurn)))]

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
