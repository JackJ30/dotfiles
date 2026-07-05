if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# Environment Variables
export PATH="$HOME/dotfiles/scripts/util:$HOME/dotfiles/scripts/system:$PATH"
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

# Create other XDG directories
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

# Declutter home
export LESSHISTFILE="${XDG_STATE_HOME}/less/history"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"
export NSS_USER_DB_DIR="${XDG_DATA_HOME}/pki"
export MYVIMRC="${XDG_CONFIG_HOME}/vim/vimrc"
export VIMINIT="source $MYVIMRC"

# Auto-start wm on tty1
if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec dbus-launch --exit-with-session mango
fi
