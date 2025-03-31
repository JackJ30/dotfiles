#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias project='. project'
PS1='\[\033[1;1m\]\u\[\033[00m\]@\[\033[34m\]\h \[\033[32m\]\w \[\033[35m\]$(git-ps1)\[\033[00m\]\$ '

export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi
