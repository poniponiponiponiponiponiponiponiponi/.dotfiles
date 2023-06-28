#!/bin/sh

DOTFILES_PATH="$HOME/.dotfiles"

mkdir -p ~/.config
mkdir -p ~/.config/mpv
mkdir -p ~/.config/tmux
mkdir -p ~/.config/i3
mkdir -p ~/.config/helix

ln -sf "$DOTFILES_PATH/mpv.conf" ~/.config/mpv/mpv.conf
ln -sf "$DOTFILES_PATH/pwninit_template.py" ~/.config/pwninit_template.py
ln -sf "$DOTFILES_PATH/tmux.conf" ~/.config/tmux/tmux.conf
ln -sf "$DOTFILES_PATH/alacritty.yml" ~/.config/alacritty.yml
ln -sf "$DOTFILES_PATH/zshrc" ~/.zshrc
ln -sf "$DOTFILES_PATH/nvim-config" ~/.config/nvim
ln -sf "$DOTFILES_PATH/sway_config" ~/.config/i3/config
ln -sf "$DOTFILES_PATH/language.toml" ~/.config/helix/language.toml
ln -sf "$DOTFILES_PATH/config.toml" ~/.config/helix/config.toml

if command -v ipython &> /dev/null
then
    ipython profile create ctf
    mkdir -p ~/.config/ipython
    ln -sf $DOTFILES_PATH/ctf_ipython_config.py ~/.config/ipython/profile_ctf/ipython_config.py
else
    echo "NO IPYTHON DETECTED"
fi

if command -v git &> /dev/null
then
    git config --global pager.branch false
else
    echo "NO GIT DETECTED"
fi
