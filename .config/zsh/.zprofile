#!/bin/zsh

# Profile runs in a login shell. Environment variables set here.

# Add ~/.local/bin to $PATH
export PATH="$PATH:${$(find ~/.local/bin -maxdepth 1 -type d -printf %p:)%%:}"

# Default Programs
export BROWSER=/usr/bin/firefox
export EDITOR="/usr/bin/emacsclient -nc"
export TERMINAL=/usr/bin/alacritty

# XDG Environment Settings
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# Additional Environment Settings
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"

export GNUPGHOME="$XDG_DATA_HOME/gnupg/"

echo "~/.zprofile has been read."
