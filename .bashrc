export _JAVA_AWT_WM_NONREPARENTING=1
alias pwninit="pwninit --template-path=$HOME/.config/pwninit_template.py"
alias v="emacsclient"
alias vi="emacsclient"
alias vim="emacsclient"
alias nvim="emacsclient"
alias ipy="ipython"
alias ls="eza"
alias l="eza -lah"

# colors
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
export LESS='-R --use-color -Dd+r$Du+b$'
export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT="-P -c"

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
. "$HOME/.cargo/env"

if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
fi

export HISTFILESIZE=999999
export HISTSIZE=9999

shopt -s checkwinsize
shopt -s histappend


