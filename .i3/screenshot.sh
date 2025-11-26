#!/bin/bash

mkdir -p $HOME/Pictures/Screenshots

OUTPUT=$HOME/Pictures/Screenshots/$(date +%Y%m%d%H%M%S).png

if [ $XDG_SESSION_TYPE = "x11" ]; then
    scrot -s $OUTPUT
else
    grim -g "$(slurp)" $OUTPUT
fi

if [ $XDG_SESSION_TYPE = "x11" ]; then
    xclip -i $OUTPUT -t image/png -selection clipboard
else
    wl-copy < $OUTPUT
fi
