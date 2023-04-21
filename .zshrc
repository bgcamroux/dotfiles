# Use powerline
#USE_POWERLINE="true"
# Source manjaro-zsh-configuration
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
