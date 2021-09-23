#!/bin/bash

mkdir -p $HOME/Pictures/Screenshots

OUTPUT=$HOME/Pictures/Screenshots/$(date +%Y%m%d%H%M%S).png

scrot -s $OUTPUT

xclip -i $OUTPUT -t image/png -selection clipboard
