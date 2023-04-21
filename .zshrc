# Use powerline
#USE_POWERLINE="true"
# Source manjaro-zsh-configuration
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

HISTSIZE=100000
SAVEHIST=100000
HISTFILE="${XDG_CACHE_HOME}/zsh/history"

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

eval "$(starship init zsh)"
