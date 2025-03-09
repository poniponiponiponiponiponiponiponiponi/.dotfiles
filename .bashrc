alias pwninit="pwninit --template-path=$HOME/.config/pwninit_template.py"
alias vi="emacsclient"
alias vim="emacsclient"
alias nvim="emacsclient"
alias nano="emacsclient"
alias py="python"
alias ipy="ipython"
alias ls="eza --icons"

# colors
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
export LESS='-R --use-color -Dd+r$Du+b$'
export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT="-P -c"

export _JAVA_AWT_WM_NONREPARENTING=1
export HISTFILESIZE=999999
export HISTSIZE=999999
export EDITOR=emacsclient
export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket

export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.local/share/gem/ruby/3.3.0/bin"
export PATH="$PATH:$HOME/.dotfiles/scripts"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/lib/rustup/bin"

RED="\[\033[0;31m\]"
GREEN="\[\033[0;32m\]"
BLUE="\[\033[0;34m\]"
YELLOW="\[\033[0;33m\]"
RESET="\[\033[0m\]"

PS1="[${GREEN}\u${RESET}@${BLUE}\h${RESET} ${YELLOW}\w${RESET}] λδ "

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

shopt -s checkwinsize
shopt -s histappend

setxkbmap -option ctrl:nocaps
setxkbmap pl
