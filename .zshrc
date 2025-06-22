#!/bin/zsh

# Completion
autoload -U compinit
compinit

# History
export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_space

# Misc
setopt autocd
setopt extendedglob

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Prompt
set_prompt() {
	PROMPT="[%B%F{174}%n%f%b%F{15}@%f%F{81}%m%f] %F{47}%~ %F{105}$(git-ps1)%F{15}$ "
}
precmd_functions+=(set_prompt)

# Vi mode
bindkey -v

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.
KEYTIMEOUT=1 # remove lag
