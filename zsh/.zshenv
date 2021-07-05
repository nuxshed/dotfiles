# a line added by the cargo install
. "$HOME/.cargo/env"

export EDITOR=nvim
export BAT_THEME=TwoDark

typeset -U PATH path

path=("$HOME/.local/bin" "$HOME/.emacs.d/bin" "$HOME/.node_modules/bin" "$HOME/.local/share/AppImages/" "$HOME/.go/bin" "$path[@]")
export PATH

export GOPATH=$HOME/.go
