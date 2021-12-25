#!/bin/sh

SESSION="decks"

tmux new-session -d -s $SESSION

tmux new-window -t $SESSION:1 -n "cli" 
tmux send-keys -t $SESSION:1 "cd cli/ && vi" Enter

tmux new-window -t $SESSION:2 -n "web" 
tmux send-keys -t $SESSION:2 "cd web/ && vi" Enter

tmux new-window -t $SESSION:3 -n "web" 
tmux send-keys -t $SESSION:3 "cd web/ && yarn start" Enter

tmux new-window -t $SESSION:4 -n "e.g." 
tmux send-keys -t $SESSION:4 "cd examples/ && vi presentation.decks" Enter

tmux attach-session -t $SESSION
