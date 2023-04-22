#!/bin/zsh

# Profile runs in a login shell. Environment variables set here.

# Add ~/.local/bin to $PATH
export PATH="$PATH:${$(find ~/.local/bin -maxdepth 1 -type d -printf %p:)%%:}"

# zsh Environment Settings
export ZDOTDIR="$HOME/.config/zsh"

echo "~/.zshenv has been read.\n"
