#!/bin/zsh

# Completion
autoload -U compinit
compinit

# History
export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_space

# Misc options
setopt autocd
setopt extendedglob
unsetopt BEEP

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

stty -ixon

# Prompt
set_prompt() {
	# PROMPT="%B%F{174}%n%f%b%F{15}@%f%F{81}%m %F{47}%~ %F{105}$(git-ps1)%F{15}$ "
	PROMPT="%B%F{153}%n %F{47}%~ %F{105}$(git-ps1)%F{15}$ "
}
precmd_functions+=(set_prompt)
