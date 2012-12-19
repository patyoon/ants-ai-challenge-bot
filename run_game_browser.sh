if [ -f MyBot ]
then mv MyBot ./
fi
python tools/playgame.py "./MyBot" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 1000 --scenario --food none --player_seed 42 --verbose -e
