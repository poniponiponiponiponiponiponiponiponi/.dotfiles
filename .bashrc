export _JAVA_AWT_WM_NONREPARENTING=1
alias pwninit="pwninit --template-path=$HOME/.config/pwninit_template.py"
alias v="emacsclient"
alias vi="emacsclient"
alias vim="emacsclient"
alias nvim="emacsclient"
alias ipy="ipython"
alias ls="exa"
alias l='exa -lah'

export PATH="$PATH:~/.cargo/bin"
export PATH="$PATH:~/.local/share/gem/ruby/3.0.0/bin"
export PATH="$PATH:~/.scripts"
export PATH="$PATH:~/.local/bin"
export PATH="$PATH:/usr/lib/rustup/bin"

RED="\[\033[0;31m\]"
GREEN="\[\033[0;32m\]"
BLUE="\[\033[0;34m\]"
YELLOW="\[\033[0;33m\]"
RESET="\[\033[0m\]"

PS1="[${GREEN}\u${RESET}@${BLUE}\h${RESET} ${YELLOW}\w${RESET}]$ "
