#!/usr/bin/fish

emacsclient -e (string join '' '(split-term "' $argv[1] '")') > /dev/null