# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. /etc/profile
export PATH="$HOME/dotfiles/scripts/:$PATH"

# Completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# History
export HISTSIZE=2000
export HISTFILESIZE=2000        # Equivalent to SAVEHIST
export HISTCONTROL=ignorespace  # Equivalent to setopt hist_ignore_space

# Misc options
shopt -s autocd     # Equivalent to setopt autocd
shopt -s extglob    # Equivalent to setopt extendedglob

# Disable Beep (Equivalent to unsetopt BEEP)
bind 'set bell-style none'

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Flow control
stty -ixon

RESET='\[\e[0m\]'
BOLD='\[\e[1m\]'
COLOR_USER='\[\e[38;5;153m\]'
COLOR_DIR='\[\e[38;5;47m\]'
COLOR_GIT='\[\e[38;5;105m\]'
COLOR_END='\[\e[38;5;15m\]'

PS1="${BOLD}${COLOR_USER}\u ${COLOR_DIR}\w ${COLOR_GIT}\$(git-ps1)${COLOR_END}$ ${RESET}"
