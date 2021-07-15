#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar
echo "---" | tee -a ~/.config/polybar/log
polybar -c ~/.config/polybar/config.ini "$1" 2>&1 | tee -a  ~/.config/polybar/log & disown 
