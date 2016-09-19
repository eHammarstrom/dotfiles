#!/bin/sh

layout=$(setxkbmap -query | grep -o "us")

echo $layout

if [ "$layout" == 'us' ]; then
  setxkbmap se -option caps:swapescape
else
  setxkbmap us -option caps:swapescape
fi
