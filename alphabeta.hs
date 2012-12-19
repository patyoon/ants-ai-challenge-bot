module AlphaBeta where
import AStar(neighbors)
import Ants(myAnts, enemyAnts)

type Depth = Int

isDraw :: World -> Ant -> Bool
isDraw w ant = undefined

-- how to compute enemydeadcount and mydeadcount.
-- how to compute combat group.
--  enemyDeadCount * 512 - myDeadCount * (512+256) - distValue;
evaluate GameState -> Int
evaluate gs = undefined

data AlphaBetaData a n = AlphaBetaData { cameFrom :: !(Map a a),
                                         myants :: [a],
                                         enemyants :: [a],
                                         depth :: Int
                                       }
b
-- generate search tree using minimax algorithm and alpha beta pruning
-- generate search tree successively for all our ants and then for all emeny ants and so on.
-- has maximum depth to stop evaluation
searchGameTree :: GameState -> [Ant] -> [Ant] -> Map Ant Point ->
                  Int -> Bool -> (Int, Int) -> (GameState, Int)
-- run minimax tree
searchGameTree gs [] enemyants depth True (alpha, beta) =
  searchGameTree (myAnts (ants gs)) enemyants (depth - 1) False (alpha, beta)
searchGameTree gs myants [] depth False (alpha, beta) =
  searchGameTree myants (enemyAnts (ants gs)) (depth - 1) False (alpha, beta)
searchGameTree gs myants@(mya:myas) enemyants@(ea:eas) depth isPlayer (alpha, beta)
  -- player's turn
  | isPlayer = if isDraw mya || depth <= 0 then (mya, evaluate mya)
               else aBPlayer neighbor (alpha, beta) node
  -- enemy's turn
  | otherwise = if isDraw ea || depth <= 0 then (ea, evaluate ea)
                else aBOpponent neighbor (alpha, beta) node where
                  neighbor = neighbors w node
                  aBPlayer :: GameState -> [Point] ->
                              (Int, Int) -> (GameState, Int)
                  aBPlayer gs [] (alpha, _) = (gs, alpha)
                  aBPlayer gs (x:xs) (alpha, beta) =
                    if beta <= $ maxVal
                    then (gs, maxVal)
                    else botSearch gs xs (maxVal, beta) where
                      (gs, value) = searchGameTree gs myas enemyants depth False (alpha, beta)
                      maxVal = max alpha value

                  aBOpponent :: GameState -> [Point] ->
                                (Int, Int) -> (GameState, Int)
                  aBOpponent gs [] (_, beta) = (gs, beta)
                  aBOpponent gs (x:xs) (alpha, beta) =
                    if alpha >= $ minVal
                    then (gs, minVal)
                    else botSearch gs xs (alpha, minVal) where
                      (gs, value) = searchGameTree gs x myants eas depth True (alpha, beta)
                      minVal = min beta value

runAlphaBeta :: GameState
                -> Maybe Order
runAlphaBeta = undefined

-- to cut-off and make a quick decision
makeDecision :: GameState -> Maybe Order
makeDecision = undefined
