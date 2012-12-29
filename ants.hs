{-# LANGUAGE GADTs #-}

-- From Haskell Starter Package
-- http://aichallenge.org/starter_packages.php

module Ants
       (
         -- Data structures
         Owner (..)
       , Ant (..)
       , Direction (..)
       , GameParams (..)
       , GameState (..)
       , Order (..)
       , World
       , Point
       , Tile (..)
       , MetaTile (..)
       , Turn (..)
       , Hill (..)
       , Food
       , (%!)
         -- Utility functions
       , myAnts -- return list of my Ants
       , enemyAnts -- return list of visible enemy Ants
       , passable
       , distance
       , directions
       , timeRemaining
       , manhattan
       , rowBound
       , colBound
       , unoccupied
       , unClaimedFood
       , game
       , move
       , moveW
       ) where

import Data.Array
import Data.List (isPrefixOf)
import Data.Char (digitToInt, toUpper)
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Applicative
import Data.Time.Clock
import System.IO

-- type synonyms
type Row = Int
type Col = Int
type ExploreValue = Int
type Visible = Bool
type Point = (Row, Col)
type Food = Point

-- Array having Point as index and MetaTile(Tile+Visible)
type World = Array Point MetaTile

-- map pointing each point to orders made at that point
-- change to array
type PointOrders = Map Point Order
type PointOrders2 = Array Point Order

-- Map mapping a food to an ant
type FoodToAnt = Map Point Point

-- Column bound of the map
colBound :: World -> Col
colBound = col . snd . bounds

-- Row bound of the map
rowBound :: World -> Row
rowBound = row . snd . bounds

-- Translates point to MetaTile (Tile+Visible)
-- Takes the modulus of the indices before accessing the array
(%!) :: World -> Point -> MetaTile
(%!) w p = w ! (w %!% p)
{-# INLINE (%!) #-}

-- get explore value of a point
getEVFromWorld :: World -> Point -> ExploreValue
getEVFromWorld w p = exploreValue $ w ! (w %!% p)

-- get a index accounting for the wrapped map edges.
(%!%) :: World -> Point -> Point
(%!%) w p =
  let modCol = 1 + colBound w
      modRow = 1 + rowBound w
      ixCol  = col p `mod` modCol
      ixRow  = row p `mod` modRow
  in (ixRow, ixCol)
{-# INLINE (%!%) #-}

-- itemgetters from Point tuple
row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

-- Objects appearing on the map
data Tile = MyAnt
          | Enemy1Ant
          | Enemy2Ant
          | Enemy3Ant
          | Dead
          | Land
          | FoodTile
          | Water
          | MyHill
          | Enemy1Hill
          | Enemy2Hill
          | Enemy3Hill
          | Unknown
          deriving (Show,Eq,Enum,Bounded)

data MetaTile = MetaTile
                { tile :: Tile,
                  visible :: Visible,
                  exploreValue :: ExploreValue
                } deriving (Show, Eq)

data Owner = Me | Enemy1 | Enemy2 | Enemy3 deriving (Show, Eq, Bounded, Enum, Ord)

data Ant = Ant
  { point :: Point
  , owner :: Owner
  -- , path :: [Point]
  } deriving (Show, Eq, Ord)

data Hill = Hill
  { hillPoint :: Point
  , hillOwner :: Owner
  } deriving (Show)

data Direction = North | East | South | West | Nothing deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"
  show Ants.Nothing = "-"

-- Representation of an order
data Order = Order
  { ant :: Ant
  , direction :: Direction
  } deriving (Show, Ord, Eq)

data GameState = GameState
  { world :: World
  , ants :: [Ant]
  , food :: [Food]
  , startTime :: UTCTime
  , hills :: [Hill]
  , unexplored :: [Point]
  }

data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int -- radius that ants can attack
  , spawnradius2 :: Int -- food gathering radius squared
  , viewPoints :: [Point]
  } deriving (Show)

data Turn = Turn
    { pointOrders :: PointOrders,
      foodToAnts :: FoodToAnt
    } deriving (Show)

--------------- Tile functions -------------------
isAnt :: Tile -> Bool
isAnt t = any (==t) [MyAnt .. Enemy3Ant]

isHill :: Tile -> Bool
isHill t = any (==t) [MyHill .. Enemy3Hill]

renderTile :: MetaTile -> String
renderTile m
  | tile m == MyAnt = visibleUpper m 'm'
  | tile m == Enemy1Ant = visibleUpper m 'a'
  | tile m == Enemy2Ant = visibleUpper m 'b'
  | tile m == Enemy3Ant = visibleUpper m 'c'
  | tile m == Dead = visibleUpper m 'd'
  | tile m == Land = visibleUpper m 'l'
  | tile m == FoodTile = visibleUpper m 'f'
  | tile m == Water = visibleUpper m 'w'
  | tile m == MyHill = visibleUpper m 'h'
  | tile m == Enemy1Hill = visibleUpper m 'd'
  | tile m == Enemy2Hill = visibleUpper m 'e'
  | tile m == Enemy3Hill = visibleUpper m 'f'
  | otherwise = "*"
  where
    visibleUpper :: MetaTile -> Char -> String
    visibleUpper mt c
      | visible mt = [toUpper c]
      | otherwise  = [c]

renderWorld :: World -> String
renderWorld w = concatMap renderAssoc (assocs w)
  where
    maxCol = colBound w
    renderAssoc :: (Point, MetaTile) -> String
    renderAssoc a
      | col (fst a) == maxCol = renderTile (snd a) ++ "\n"
      | otherwise = renderTile (snd a)

-- mod distance between x and y by n. Used for wrapping the grid.
modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)

manhattan :: Point -- bound
          -> Point -> Point -> Int
manhattan bound p1 p2 =
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in rowd + cold

oneNorm :: Point -> Int
oneNorm p = row p + col p

twoNormSquared :: Point -> Int
twoNormSquared p = row p ^ 2 + col p ^ 2

euclidSquare :: Point  -- bound
             -> Point -> Point -> Int
euclidSquare bound p1 p2 =
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in (rowd ^ 2) + (cold ^ 2)

-- calculate the closest distance between to locations
distance :: GameParams -> Point -> Point -> Int
distance gp l1 l2 =
  let maxRow = rows gp - 1
      maxCol = cols gp - 1
      rowDist = modDistance maxRow (row l1) (row l2)
      colDist = modDistance maxCol (col l1) (col l2)
  in rowDist + colDist

-- pair of vertical and/or horizontal directions the to reach to reach a location
directions :: World -> Point -> Point -> (Direction, Direction)
directions world source dest
  | y1 == y2 = (xdir, Ants.Nothing)
  | x1 == x2 = (Ants.Nothing, ydir)
  | otherwise = (xdir, ydir) where
    x1 = row source
    x2 = row dest
    y1 = col source
    y2 = col dest
    -- the world is wrapping up.
    xdir = if (abs $ x1 - x2) > ((rowBound world) `div` 2)
           then if x1 >= x2 then South else North
           else if x1 >= x2 then North else South
    ydir = if (abs $ y1 - y2) > ((colBound world) `div` 2)
           then if y1 >= y2 then East else West
           else if y1 >= y2 then West else East

-- is my ant?
isMe :: Ant -> Bool
isMe a = owner a == Me

-- Get only my ants
myAnts :: [Ant] -> [Ant]
myAnts = filter isMe

-- is enemy ant?
isEnemy :: Ant -> Bool
isEnemy = not . isMe

-- Get only enemy ants
enemyAnts :: [Ant] -> [Ant]
enemyAnts = filter isEnemy

-- is my hill?
isMyHill :: Hill -> Bool
isMyHill h = hillOwner h == Me

-- Get only my hills
myHills :: [Hill] -> [Hill]
myHills = filter isMyHill

-- simulate move in dir and get the point after move
move :: Direction -> Point -> Point
move dir p
  | dir == Ants.Nothing = p
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)
{-# INLINE move #-}

-- simulate move in dir and get the point after move
moveW :: World -> Direction -> Point -> Point
moveW w dir p
  = let ixCol p = if col p >= 0 then col p `mod` (1 + colBound w)
                  else col p + (1 + colBound w)
        ixRow p = if row p >= 0 then row p `mod` (1 + rowBound w)
                  else row p + (1+ rowBound w)
        wrap p = (ixRow p, ixCol p)
    in case dir of
      Ants.Nothing -> wrap p
      North -> wrap (row p - 1, col p)
      South -> wrap (row p + 1, col p)
      West  -> wrap (row p, col p - 1)
      East  -> wrap (row p, col p + 1)
{-# INLINE moveW #-}

-- tests if the order is a valid one.
passable :: World -> Order -> Bool
passable w order
  | (direction order) /= Ants.Nothing = tile (w %! next) `elem` [Land, Dead]
  | otherwise =  False where
    next = move (direction order) (point $ ant order)

-- if newPoint is not occupied add the order to Turn
unoccupied :: Turn -> Order -> Turn
unoccupied gt order =
  Turn {pointOrders = newOrders, foodToAnts = foodToAnts gt}
  where newPoint = move (direction order) (point (ant order))
        newOrders = if Map.notMember newPoint (pointOrders gt)
                    then Map.insert newPoint order (pointOrders gt)
                    else pointOrders gt

-- if Food and the order of the food is not occupied and not issued to other ants
-- add the order to Turn
unClaimedFood :: Turn -> (Food, Maybe Order) -> Turn
unClaimedFood gt foodorder =
    if (not (isNothing (snd foodorder)))
    then
        let food_loc = fst foodorder
            order = fromMaybe Order {ant = Ant{point = (0, 0), owner = Me}, direction = North}  (snd foodorder)
            notSentForFood = (Map.notMember food_loc (foodToAnts gt)) && ((point (ant order)) `notElem` (Map.elems (foodToAnts gt)))
            new_gt = if notSentForFood then unoccupied gt order else gt
            newFoodToAnt = if notSentForFood
                               then Map.insert food_loc (point (ant order)) (foodToAnts gt)
                               else foodToAnts gt
        in Turn {pointOrders = pointOrders new_gt, foodToAnts = newFoodToAnt}
    else gt

-- issue order by writing it to stdout
issueOrder :: Order -> IO ()
issueOrder order = do
  let srow = (show . row . point . ant) order
      scol = (show . col . point . ant) order
      sdir = (show . direction) order
  putStrLn $ "o " ++ srow ++ " " ++ scol ++ " " ++ sdir

-- write go at the last line
-- 'go' signals that the player finished writing orders.
finishTurn :: IO ()
finishTurn = do
  putStrLn "go"
  hFlush stdout

-- make 2-element list into a tuple
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

-- maps Oweer type to Hill Tile type
ownerToHill :: Owner -> Tile
ownerToHill Me = MyHill
ownerToHill Enemy1 = Enemy1Hill
ownerToHill Enemy2 = Enemy2Hill
ownerToHill Enemy3 = Enemy2Hill

-- maps Owner type to Ant Tile type
ownerToAnt :: Owner -> Tile
ownerToAnt Me = MyAnt
ownerToAnt Enemy1 = Enemy1Ant
ownerToAnt Enemy2 = Enemy2Ant
ownerToAnt Enemy3 = Enemy2Ant

-- maps integer input from stdin to Owner type
toOwner :: Int -> Owner
toOwner 0 = Me
toOwner 1 = Enemy1
toOwner 2 = Enemy2
toOwner _ = Enemy3

-- adds food to gamestate
addFood :: GameState -> Point -> GameState
addFood gs loc =
  GameState {world = newWorld, ants = ants gs,
             hills = hills gs, food = newFood,
             unexplored = unexplored gs List.\\ [loc],
             startTime = startTime gs} where
    newFood = loc:food gs
    w = world gs
    ev = getEVFromWorld w loc
    newWorld = w // [(loc, MetaTile {tile = FoodTile, visible = True,
                                                exploreValue = ev})]

-- Sums up two points
sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

-- adds visible to gamestate
addVisible :: World
           -> [Point] -- viewPoints
           -> Point -- location
           -> World
addVisible w vp p =
  let vis = map (sumPoint p) vp
      vtuple :: Point -> (Point, MetaTile)
      vtuple pt = (w %!% pt, visibleMetaTile $ w %! pt)
  in w // map vtuple vis

addSeen :: [Point] -> [Point] -> Point ->[Point]
addSeen vp s p =
  let vis = map (sumPoint p) vp
  in s List.\\ vp

-- add ant
addAnt :: GameParams -> GameState -> Point -> Owner -> GameState
addAnt gp gs p own =
  GameState {world = newWorld, ants = newAnts, food = food gs,
             hills = hills gs,
             unexplored = (unexplored gs) List.\\ (map (sumPoint p) (viewPoints gp)) ,
             startTime = startTime gs} where
    newAnts   = Ant {point = p, owner = own}:ants gs
    newWorld' = if own == Me
                then addVisible (world gs) (viewPoints gp) p
                else world gs
    ev = getEVFromWorld (world gs) p
    newWorld  = newWorld' // [(p, MetaTile {tile = ownerToAnt own, visible = True,
                                            exploreValue = ev})]
-- add dead ants to map
addDead :: GameParams -> GameState -> Point -> Owner -> GameState
addDead gp gs p own =
  GameState {world = newWorld, ants = ants gs,
             hills = hills gs, food = food gs,
             unexplored = (unexplored gs) List.\\ (map (sumPoint p) (viewPoints gp)),
             startTime = startTime gs} where
    newWorld' = if own == Me
                then addVisible (world gs) (viewPoints gp) p
                else world gs
    ev = getEVFromWorld (world gs) p
    newWorld = newWorld' // [(p, MetaTile {tile = Dead, visible = True,
                                           exploreValue = ev})]

-- added to add hills
addHill :: GameState -> Point -> Owner-> GameState
addHill gs p own =
   let newHills  = Hill {hillPoint = p, hillOwner = own}:hills gs
       ev = getEVFromWorld (world gs) p
       newWorld  = world gs // [(p, MetaTile {tile = ownerToHill own,
                                              visible = True, exploreValue = ev})]
  in GameState {world = newWorld, ants = ants gs,
                food = food gs,
                unexplored = unexplored gs List.\\ [p], hills = newHills,
                startTime = startTime gs}

-- add Tile t to GameState at Point p
-- if replacing a visible tile it should be kept visible
addWorldTile :: GameState -> Tile -> Point -> GameState
addWorldTile gs t p =
  let ev = getEVFromWorld (world gs) p
      newWorld = world gs // [(p, MetaTile {tile = t, visible = True,
                                            exploreValue = ev})]
  in GameState {world = newWorld, ants = ants gs,
                hills = hills gs, food = food gs,
                unexplored = (unexplored gs) List.\\ [p],
                startTime = startTime gs}

-- initialize GameState
initialGameState :: GameParams -> UTCTime -> GameState
initialGameState gp time =
  let w = listArray ((0,0), (rows gp - 1, cols gp - 1)) (repeat MetaTile {tile = Unknown, visible = False, exploreValue = 0})
  in GameState {world = w, ants = [], food = [], hills = [], unexplored = [(row, col) | row <- [0 .. (rows gp -1)], col <- [0 .. (cols gp - 1)]], startTime = time}

-- Update GameState with character information provided from STDIN.
updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s
  | "f" `isPrefixOf` s = addFood gs $ toPoint . tail $ s
  | "w" `isPrefixOf` s = addWorldTile gs Water $ toPoint . tail $ s
  | "a" `isPrefixOf` s = addAnt gp gs (toPoint . init . tail $ s) (toOwner . digitToInt . last $ s)
  | "d" `isPrefixOf` s = addDead gp gs (toPoint . init . tail $ s) (toOwner . digitToInt . last $ s)
  | "h" `isPrefixOf` s = addHill gs (toPoint . init . tail $ s) (toOwner . digitToInt . last $ s)
  | otherwise = gs -- ignore line
  where
    toPoint :: String -> Point
    toPoint = tuplify2 . map read . words

-- increment explore value by 1 in the beginning of the turn
-- set explore value to 0 for tiles within 10 steps
updateExplore :: GameParams -> GameState ->  GameState
updateExplore gp gs =
  let withinTen = (filter (filterStep gp gs False)
                   (assocs $ world gs))
      newWorld = world gs // [(loc, MetaTile {tile = tile mt,
                                              visible = visible mt, exploreValue =
                                                exploreValue mt + 1})
                             | (loc, mt) <- ((assocs $ world gs) List.\\ withinTen)]
      newWorld'  = newWorld // [(loc, MetaTile {tile = tile mt,
                                                 visible = visible mt, exploreValue = 0})
                                | (loc, mt) <-  withinTen]
  in gs {world = newWorld'}
{-# INLINE updateExplore #-}

filterStep :: GameParams -> GameState -> Bool -> (Point, MetaTile) -> Bool
filterStep gp gs filterTen (p, _)
  | null distList = filterTen
  | filterTen && dist > 5 = True
  | not filterTen && dist <= 5 = True
  | otherwise = False where
    myants = myAnts (ants gs)
    distList = List.sort $map ((distance gp p) . point) myants
    dist =  (head distList)
{-# INLINE filterStep #-}

-- increment explore value by 1 in the beginning of the turn
incrementExplore :: GameState ->  GameState
incrementExplore gs =
  let newWorld  = world gs // [(loc, MetaTile {tile = tile mt,
                                               visible = visible mt, exploreValue =
                                                 exploreValue mt + 1})
                              | (loc, mt) <- (assocs $ world gs),
                                tile mt `notElem` [MyAnt, MyHill, Water]]
  in gs {world = newWorld}
{-# INLINE incrementExplore #-}

-- initialize exploreValue to 0
initExploreValue :: GameState -> Set Point -> GameState
initExploreValue gs setp
  = let newWorld = world gs // [(loc, MetaTile {tile = tile mt,
                                                visible = visible mt,
                                                exploreValue = 0}) |
                                loc <- Set.toList setp,
                                let mt = (world gs) %! loc]
    in gs {world = newWorld}
{-# INLINE initExploreValue #-}

-- recursively read input
updateGame :: GameParams -> GameState -> IO GameState
updateGame gp gs = do
  line <- getLine
  process line
  where
    process line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          updateGame gp gs
      | "go" `isPrefixOf` line   = do
          currentTime <- getCurrentTime
          return GameState {world = world gs
                           , ants = ants gs
                           , food = food gs
                           , hills = hills gs
                           , unexplored = unexplored gs
                           , startTime = currentTime
                           }
      | otherwise = updateGame gp $ updateGameState gp gs line

-- Sets the tile to visible
-- If the tile is still Unknown then it is land
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m
  | tile m == Unknown = MetaTile {tile = Land, visible = True, exploreValue = ev}
  | otherwise         = MetaTile {tile = tile m, visible = True, exploreValue = ev} where
    ev = exploreValue m

-- check if all boolean functions in x is True
fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

-- check if any of boolean functions in x is True
fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

-- Resets Tile to Land if it is currently occupied by food or ant
-- and makes the tile invisible
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m
  | fOr (tile m) [isAnt, isHill, (==FoodTile), (==Dead)] = MetaTile {tile = Land, visible = False, exploreValue = ev}
  | otherwise = MetaTile {tile = tile m, visible = False, exploreValue = ev}
  where ev = exploreValue m

-- Clears ants and food and sets tiles to invisible
cleanState :: GameState -> GameState
cleanState gs =
  GameState {world = nw, ants = [], food = [], hills = [], unexplored = unexplored gs, startTime = startTime gs}
  where
    w = world gs
    invisibles = map clearMetaTile $ elems w
    nw = listArray (bounds w) invisibles

timeRemaining :: GameState -> IO NominalDiffTime
timeRemaining gs = do
  timeNow <- getCurrentTime
  return $ timeNow `diffUTCTime` startTime gs

gatherParamInput :: IO [String]
gatherParamInput = gatherInput' []
  where
    gatherInput' :: [String] -> IO [String]
    gatherInput' xs = do
      line <- getLine
      if "ready" /= line
        then gatherInput' (line:xs)
        else return xs

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      lt  = lookup' "loadtime"
      tt  = lookup' "turntime"
      rs  = lookup' "rows"
      cs  = lookup' "cols"
      ts  = lookup' "turns"
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      mx  = truncate $ sqrt $ fromIntegral vr2
      vp' = (,) <$> [-mx..mx] <*> [-mx..mx]
      vp  = filter (\p -> twoNormSquared p <= vr2) vp'
  in GameParams { loadtime      = lt
                , turntime      = tt
                , rows          = rs
                , cols          = cs
                , turns         = ts
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewPoints    = vp
                }

endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)

gameLoop :: GameParams -> GameState
            -> (GameParams -> GameState -> IO ([Order], Set Point))
                -> IO ()
gameLoop gp gs doTurn = do
  line <- getLine
  gameLoop' line
  where
    gameLoop' line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          let gsc =  (cleanState $ incrementExplore gs)
          gsu <- updateGame gp gsc
          (orders, explore_set) <- doTurn gp gsu
          -- set explore value to 0 for tiles within 10 steps
          let gsu' = initExploreValue gsu explore_set
          hPutStrLn stderr $ show orders
          mapM_ issueOrder orders
          finishTurn
          gameLoop gp gsu' doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

game :: (GameParams -> GameState -> IO ([Order], Set Point)) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ map (tuplify2 . words) paramInput
  currentTime <- getCurrentTime
  let gs = initialGameState gp currentTime
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn