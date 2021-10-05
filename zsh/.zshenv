# .zshenv

# cargo
. "$HOME/.cargo/env"

# exports
export EDITOR=nvim
export MANPAGER="less -R --use-color -Dd+r -Du+b -DS+ky -DP+kg -DE+kR"
export VISUAL=/bin/vim
export DOTFILES=$HOME/dotfiles
export PROJECTS_DIR=$HOME/projects

typeset -U PATH path

path=("$HOME/.bin" "$HOME/.local/bin" "$HOME/.emacs.d/bin" "$HOME/.node_modules/bin" "$path[@]")
export PATH

if [ -d "$HOME/n" ]; then
  export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"
fi

export PATH=$N_PREFIX/bin:$PATH
