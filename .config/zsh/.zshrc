##########
##
## .zshrc
##
## Configuration for Brad Camroux on Pluto
## with inspiration from Manjaro Project, Eric Murphy (ericmurphy.xyz),
## and Luke Smith.
##
## Attribution-ShareAlike 4.0 International
## CC BY-SA 4.0
##
##########

# Options
setopt correct                                                  # Auto correct mistakes
setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt rcexpandparam                                            # Array expension with parameters
setopt nocheckjobs                                              # Don't warn about running processes when exiting
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt appendhistory                                            # Immediately append history instead of overwriting
setopt histignorealldups                                        # If a new command is a duplicate, remove the older one
setopt autocd                                                   # if only directory path is entered, cd there.

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh/cache

WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word

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
# XDG System Directories
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

# Additional Environment Settings
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"

export GNUPGHOME="$XDG_DATA_HOME/gnupg/"


export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

HISTSIZE=100000
SAVEHIST=100000
HISTFILE="${XDG_CACHE_HOME}/zsh/history"

# Aliases
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias cp="cp -i"                                       # Interactive mode for copying
alias df="df -h"                                       # Human-readable sizes

## Keybindings
bindkey -e
bindkey '^[[7~'  beginning-of-line                     # Home key
bindkey '^[[H'   beginning-of-line                     # Home key
if [[ "${terminfo[khome]}" != "" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line       # [Home] - Go to beginning of line
fi
bindkey '^[[8~' end-of-line                            # End key
bindkey '^[[F' end-of-line                             # End key
if [[ "${terminfo[kend]}" != "" ]]; then
  bindkey "${terminfo[kend]}" end-of-line              # [End] - Go to end of line
fi
bindkey '^[[2~' overwrite-mode                         # Insert key
bindkey '^[[3~' delete-char                            # Delete key
bindkey '^[[C'  forward-char                           # Right key
bindkey '^[[D'  backward-char                          # Left key
bindkey '^[[5~' history-beginning-search-backward      # Page up key
bindkey '^[[6~' history-beginning-search-forward       # Page down key

# Navigate words with ctrl+arrow keys
bindkey '^[Oc' forward-word                            #
bindkey '^[Od' backward-word                           #
bindkey '^[[1;5D' backward-word                        #
bindkey '^[[1;5C' forward-word                         #
bindkey '^H' backward-kill-word                        # delete previous word with ctrl+backspace
bindkey '^[[Z' undo                                    # Shift+tab undo last action

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r

# Plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up			
bindkey '^[[B' history-substring-search-down

# Use the Starship prompt
eval "$(starship init zsh)"

echo "~/.config/.zshrc has been run.\n"
