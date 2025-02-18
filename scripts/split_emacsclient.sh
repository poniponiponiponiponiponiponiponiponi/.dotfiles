CMD=$(printf "%q" "$*")
emacsclient -e "(split-eat \"$CMD\")" > /dev/null
