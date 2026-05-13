if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# Environment Variables
export EDITOR="/bin/emacs"

# Create XDG Runtime Dir
if test -z "${XDG_RUNTIME_DIR}"; then
    export XDG_RUNTIME_DIR=/run/user/${UID}
fi
if test -d "${XDG_RUNTIME_DIR}"; then
    perms="$(stat -c '%a %u' "${XDG_RUNTIME_DIR}")"
    if [[ "${perms}" != "700 ${UID}" ]]; then
        export -n XDG_RUNTIME_DIR
        echo "WARNING! XDG_RUNTIME_DIR has incorrect permissions"
    fi
else
    mkdir -p "${XDG_RUNTIME_DIR}"
    chmod 0700 "${XDG_RUNTIME_DIR}"
fi

# Auto-start wm on tty1
if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec mango
fi

