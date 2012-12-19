if [ -f MyBot ]
then mv MyBot ./
fi
cd tools
./playgame.py -So --player_seed 42 --end_wait=0.25 --verbose --log_dir ./game_logs --turns 3000 --map_file ./maps/maze/maze_04p_01.map "$@" \
    "./MyBot" \
    "python ./sample_bots/python/LeftyBot.py" \
    "python ./sample_bots/python/HunterBot.py" \
    "python ./sample_bots/python/LeftyBot.py" |
java -jar ./visualizer.jar
