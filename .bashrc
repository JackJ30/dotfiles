# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# History
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL=ignorespace

# Misc options
shopt -s autocd

# Disable Beep
bind 'set bell-style none'

# Aliases that significantly change function
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -Iv"
alias mkdir="mkdir -pv"

# Juice Aliases
alias ls='ls -hN --color=auto --group-directories-first'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip -color=auto'
alias tree='tree --gitignore -I .git'

# disable xon/xoff (C-s and C-q)
stty -ixon

# set PS1
RESET='\[\e[0m\]'
BOLD='\[\e[1m\]'
COLOR_USER='\[\e[38;5;153m\]'
COLOR_DIR='\[\e[38;5;47m\]'
COLOR_GIT='\[\e[38;5;105m\]'
COLOR_END='\[\e[38;5;15m\]'
PS1="${BOLD}${COLOR_USER}\u ${COLOR_DIR}\w ${COLOR_GIT}\$(git-ps1)${COLOR_END}$ ${RESET}"
