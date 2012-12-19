rm -f ./src/*.hi ./src/*.o
ghc --make ./MyBot.hs ./Ants.hs ./astar.hs ./Data/PSQueue.hs ./bfs.hs
