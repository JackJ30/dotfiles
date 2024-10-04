#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='\[\033[1;1m\]\u\[\033[00m\]@\[\033[34m\]\h \[\033[32m\]\w \[\033[35m\]$(git-ps1)\[\033[00m\]\$ '

