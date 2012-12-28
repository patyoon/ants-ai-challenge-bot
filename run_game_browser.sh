 #!/usr/bin/env sh
if [ -f MyBot ]
then mv MyBot ./tools
fi
cd tools
./playgame.py --player_seed 42 --verbose --log_dir game_logs --log_output -e --turns 200 -o -v --map_file maps/maze/maze_04p_01.map "$@" "./MyBot" "python sample_bots/python/LeftyBot.py" "python sample_bots/python/HunterBot.py" "python sample_bots/python/GreedyBot.py"
