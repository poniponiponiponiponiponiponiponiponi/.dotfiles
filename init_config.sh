#!/bin/sh

DOTFILES_PATH="$HOME/.dotfiles"

mkdir -p ~/.config
mkdir -p ~/.config/mpv
mkdir -p ~/.config/tmux

ln -sf "$DOTFILES_PATH/mpv.conf" ~/.config/mpv/mpv.conf
ln -sf "$DOTFILES_PATH/pwninit_template.py" ~/.config/pwninit_template.py
ln -sf "$DOTFILES_PATH/tmux.conf" ~/.config/tmux/tmux.conf
ln -sf "$DOTFILES_PATH/alacritty.yml" ~/.config/alacritty.yml
ln -sf "$DOTFILES_PATH/zshrc" ~/.zshrc
ln -sf "$DOTFILES_PATH/nvim-config" ~/.config/nvim

if command -v ipython &> /dev/null
then
    ipython profile create ctf
    mkdir -p ~/.config/ipython
    ln -sf $DOTFILES_PATH/ctf_ipython_config.py ~/.config/ipython/profile_ctf/ipython_config.py
else
    echo "NO IPYTHON DETECTED"
fi
