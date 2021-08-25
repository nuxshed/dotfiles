# .zshenv

# cargo
. "$HOME/.cargo/env"

# exports
export EDITOR=nvim
export VISUAL=/bin/vim
export DOTFILES=$HOME/dotfiles
export PROJECTS_DIR=$HOME/projects

# PATH
typeset -U PATH path

path=("$HOME/.bin" "$HOME/.local/bin" "$HOME/.emacs.d/bin" "$HOME/.node_modules/bin" "$HOME/.local/share/AppImages/" "$HOME/.go/bin" "$path[@]")
export PATH

if [ -d "$HOME/n" ]; then
  export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"
fi

export PATH=$N_PREFIX/bin:$PATH
export GOPATH=$HOME/.go
