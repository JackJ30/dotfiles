export PATH="$HOME/dotfiles/scripts/:/usr/local/lib:$HOME/path:$HOME/opt/Odin:$PATH"
export EDITOR="/bin/emacs"
export GHIDRA_INSTALL_DIR="/usr/local/bin/ghidraFiles"

if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec dbus-launch --exit-with-session Hyprland
fi
