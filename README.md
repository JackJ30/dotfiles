# Jack Jamison's Dotfiles

## Dependencies
System setup:
- Zsh is set as shell
- Elogind is running

Important
- Hyprland + utils (hyprpaper, hypridle, hyprlock, hyprpicker)
- Zsh
- Alacritty
- Swenu
- Emacs ("tree-sitter -X gtk dynamic-loading" use flags for gentoo)

Helper
- Brightnessctl
- Grim and Slurp for screenshots
- Ripgrep

Fonts and style:
- Whiteglass xcursor theme
- Dejavu Sans Mono
- Nerd icons

## Installation
Config files can be installed by running `stow .` Since there are probably conflicts with your existing files, you can use the `--adopt` flag and then `git restore .`.

## TODO
- [x] Git branch in PS1
- [x] Hyprpaper
- [x] Cursor
- [x] Status bar
- [x] Screenshot
- [x] Idle
- [x] Sound
- [x] Improve zsh completion and highlighting
- [ ] Emacs vterm and projectile detached/tmux like workflow
