#!/bin/bash

selected=$(wmctrl -l | awk '{$1=$1; $2=$2; $3=$3; sub(/^[^ ]+ [^ ]+ [^ ]+ /,""); print $0}' | dmenu -i -l 20 -nb "#212026" -nf "#b2b2b2" -sb "#bc6ec5" -sf "#212026" -fn "JetBrainsMono Nerd Font:size=12")
win_id=$(wmctrl -l | grep -F "$selected" | awk '{print $1}')
wmctrl -i -a "$win_id"