if [ -f MyBot ]
then mv MyBot ./tools
fi
cd tools
./playgame.py --engine_seed 42 --player_seed 42 --food none --verbose --log_dir game_logs --turns 30 --map_file submission_test/test.map "./MyBot" "python submission_test/TestBot.py" --nolaunch -e --strict --capture_errors
