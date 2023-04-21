#!/bin/zsh

# Profile runs on login. Environment variables set here

# Add ~/.local/bin to $PATH
export PATH="$PATH:/home/brad/.local/bin"

# Default Programs
export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/emacsclient
export TERMINAL=/usr/bin/alacritty

# XDG Environment Settings
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Additional Environment Settings
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"
