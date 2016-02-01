#!/bin/sh

i3status | while :
do
  layout=$(setxkbmap -query | grep -o -E '(us|se)')
  dataDown=$(ifconfig eno1 | grep -o -m 1 -E '(\(.* MiB\)|\(.* GiB\))' | head -1)
  dataUp=$(ifconfig eno1 | grep -o -m 2 -E '(\(.* MiB\)|\(.* GiB\))' | tail -1)
  disk=$(df -hl | grep -w "/" | awk '{print $4}')
  time=$(date | awk '{print $4}')

  echo "  ${layout^^}  |  D: $dataDown  |  U: $dataUp  |  $disk  |  $time  " || exit 1
done
