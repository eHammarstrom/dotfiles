#!/bin/sh
cd ~/git

dev=development

tmux has-session -t $dev 2>/dev/null

if [ $? -eq 1 ];
then
	tmux start-server
	tmux new-session -d -s $dev -n code
	tmux split-window -v -p 25 -t $dev:0
	tmux split-window -h -t $dev:0.0
	tmux new-window -t $dev:1 -n other
	tmux select-window -t $dev:0
fi

tmux attach-session -d -t $dev
