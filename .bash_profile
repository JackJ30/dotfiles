#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="$PATH:$HOME/dotfiles/scripts"
export PATH="$PATH:$HOME/dotfiles/shortcuts"

if [[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]]; then
    exec dbus-launch --exit-with-session Hyprland
fi
