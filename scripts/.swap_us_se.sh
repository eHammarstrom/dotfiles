#!/bin/sh

layout=$(setxkbmap -query | grep -o "us")

echo $layout

if [ "$layout" == 'us' ]; then
  setxkbmap se
else
  setxkbmap us
fi
