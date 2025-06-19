export PATH="$HOME/dotfiles/scripts/:$PATH"

if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec dbus-launch --exit-with-session Hyprland
fi
