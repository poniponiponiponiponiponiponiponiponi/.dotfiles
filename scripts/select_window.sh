#!/bin/bash

selected=$(wmctrl -l | awk '{$1=$1; $2=$2; $3=$3; sub(/^[^ ]+ [^ ]+ [^ ]+ /,""); print $0}' | dmenu -i -l 20 -nb "#282828" -nf "#ebdbb2" -sb "#83a598" -sf "#282828" -fn "JetBrainsMono Nerd Font:size=12")
win_id=$(wmctrl -l | grep -F "$selected" | awk '{print $1}')
wmctrl -i -a "$win_id"