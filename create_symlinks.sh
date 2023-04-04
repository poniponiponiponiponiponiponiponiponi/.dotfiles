#!/bin/bash

DOTFILES_PATH="$HOME/.dotfiles"

mkdir -p ~/.config
mkdir -p ~/.config/mpv

ln -s "$DOTFILES_PATH/mpv.conf" ~/.config/mpv/mpv.conf
ln -s "$DOTFILES_PATH/pwninit_template.py" ~/.config/pwninit_template.py
ln -s "$DOTFILES_PATH/tmux.conf" ~/.config/tmux.conf
ln -s "$DOTFILES_PATH/alacritty.yml" ~/.config/alacritty.yml
ln -s "$DOTFILES_PATH/.zshrc" ~/.zshrc
