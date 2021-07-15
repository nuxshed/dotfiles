# ~/.zshrc

# written by zsh-newuser-install
HISTFILE=~/.zsh_hist
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

zstyle :compinstall filename '/home/advait/.zshrc'

autoload -Uz compinit promptinit
compinit -i
promptinit

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

setopt auto_cd

#aliases
alias vim="nvim"
alias vi="/bin/vim"
alias ls="exa"
alias la="exa -a"
alias li="exa --icons"
alias cdls="cd '$@' && ls"
alias btctl="bluetoothctl"
alias cat="bat"
alias grep="grep --color=auto" 
alias wallpaper="feh --bg-scale"
alias clear='/bin/clear && printf "\033[3J"'
alias clearscreen="/bin/clear"
alias brightness="brightnessctl set"
alias rotatescreen="xrandr --output eDP-1 --rotate"
alias weather="curl 'wttr.in/?T'"
alias icat="kitty +kitten icat"
alias qtilechk="python2 -m py_compile ~/.config/qtile/config.py"
alias luamake=/home/advait/.local/share/lua-language-server/3rd/luamake/luamake

# jump to project
alias proj="z projects"

# kitty completion
kitty + complete setup zsh | source /dev/stdin

# autojump
source /etc/profile.d/autojump.zsh

# z
[[ -r "/usr/share/z/z.sh" ]] && source /usr/share/z/z.sh

# Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

if [[ "$TERM" != "linux" ]]; then
  zinit light zsh-users/zsh-autosuggestions
  zinit light zdharma/fast-syntax-highlighting

  eval "$(starship init zsh)"
elif [[ "$TERM" == "linux" ]]; then
  prompt suse
fi
