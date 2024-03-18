fish_add_path /home/tabun-dareka/.cargo/bin
fish_add_path /home/tabun-dareka/.local/share/gem/ruby/3.0.0/bin
fish_add_path /home/tabun-dareka/.scripts
fish_add_path /home/tabun-dareka/.local/bin
fish_add_path /home/poni/.cargo/bin
fish_add_path /home/poni/.local/share/gem/ruby/3.0.0/bin
fish_add_path /home/poni/.scripts
fish_add_path /home/poni/.local/bin
alias pwninit='pwninit --template-path=/home/poni/.dotfiles/pwninit_template.py'
alias l='ls -a'
alias ipy='ipython'
alias m='math'
set -x DEBUGINFOD_URLS "https://debuginfod.elfutils.org/"

if status is-interactive
    # Commands to run in interactive sessions can go here
end



# below all the vterm emacs things

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

function fish_title
    hostname
    echo ":"
    prompt_pwd
end

function emacsclient_launch
	 emacsclient -e (string join '' '(vterm_cmd split-term ' $argv[1] ')')
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end

function vterm_cmd --description 'Run an Emacs command among the ones been defined in vterm-eval-cmds.'
    set -l vterm_elisp ()
    for arg in $argv
        set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
    end
    vterm_printf '51;E'(string join '' $vterm_elisp)
end

function e
    set -q argv[1]; or set argv[1] "."
    vterm_cmd find-file (realpath "$argv")
end

function say
    vterm_cmd message "%s" "$argv"
end
