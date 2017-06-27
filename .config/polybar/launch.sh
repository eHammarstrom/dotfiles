#!/usr/bin/env sh

killall -p polybar

while pgrep -x polybar >/dev/null; do sleep 1; done

polybar theBar &

echo "Bars launched..."
