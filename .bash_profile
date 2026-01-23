if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# Environment Variables
export EDITOR="/bin/emacs"

# Auto-start sway on tty1
if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec sway
fi
