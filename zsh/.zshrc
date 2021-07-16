# ~/.zshrc

# profiling
PROFILE_STARTUP=true
if [[ "$PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/.zsh_startlog.$$
    setopt xtrace prompt_subst
fi


# zsh options
bindkey -e

zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit promptinit
compinit -i
promptinit

# unsetopt case_glob            # Use Case-Insensitve Globbing.
setopt globdots                 # Glob Dotfiles As Well.
setopt extendedglob             # Use Extended Globbing.
setopt auto_cd                  # no need to specify cd
setopt correct                  # Turn On Corrections

# Completion Options.
setopt complete_in_word         # Complete From Both Ends Of A Word.
setopt always_to_end            # Move Cursor To The End Of A Completed Word.
setopt path_dirs                # Perform Path Search Even On Command Names With Slashes.
setopt auto_menu                # Show Completion Menu On A Successive Tab Press.
setopt auto_list                # Automatically List Choices On Ambiguous Completion.
setopt auto_param_slash         # If Completed Parameter Is A Directory, Add A Trailing Slash.
setopt no_complete_aliases

setopt menu_complete            # Do Not Autoselect The First Completion Entry.
unsetopt flow_control           # Disable Start/Stop Characters In Shell Editor.

# zstyle
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':fzf-tab:*' query-string prefix first
zstyle ':fzf-tab:*' continuous-trigger '/'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm,cmd -w -w"
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':fzf-tab:*' popup-pad 0 0
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:exa' file-sort modification
zstyle ':completion:*:exa' sort false

# History
HISTFILE=~/.zsh_hist
HISTSIZE=100000
SAVEHIST=5000
setopt appendhistory notify
unsetopt beep nomatch                # beeps are annoying.

setopt bang_hist                # Treat The '!' Character Specially During Expansion.
setopt inc_append_history       # Write To The History File Immediately, Not When The Shell Exits.
setopt share_history            # Share History Between All Sessions.
setopt hist_expire_dups_first   # Expire A Duplicate Event First When Trimming History.
setopt hist_ignore_dups         # Do Not Record An Event That Was Just Recorded Again.
setopt hist_ignore_all_dups     # Delete An Old Recorded Event If A New Event Is A Duplicate.
setopt hist_find_no_dups        # Do Not Display A Previously Found Event.
setopt hist_ignore_space        # Do Not Record An Event Starting With A Space.
setopt hist_save_no_dups        # Do Not Write A Duplicate Event To The History File.
setopt hist_verify              # Do Not Execute Immediately Upon History Expansion.
setopt extended_history         # Show Timestamp In History.

# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh``

# aliases
alias vim="nvim"
alias vi="/bin/vim"
alias ls="exa"
alias la="exa -a"
alias li="exa --icons"
alias lg="lazygit"
alias btctl="bluetoothctl"
alias pb="nc termbin.com 9999"
alias cat="bat"
alias grep="grep --color=auto" 
alias clear='/bin/clear && printf "\033[3J"'
alias clearscreen="/bin/clear"
alias brightness="brightnessctl set"
alias rotatescreen="xrandr --output eDP-1 --rotate"
alias icat="kitty +kitten icat"
alias luamake=/home/advait/.local/share/lua-language-server/3rd/luamake/luamake

# kitty completion
kitty + complete setup zsh | source /dev/stdin

# autojump
source /etc/profile.d/autojump.zsh

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
# end of block added by zinit's installer

# These plugins provide many aliases
zinit wait lucid for \
        OMZ::lib/git.zsh \
    atload"unalias grv" \
        OMZ::plugins/git/git.plugin.zsh

zinit lucid wait'0a' for \
  as"program" pick"$ZPFX/bin/git-*" src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras

zinit light-mode for \
  OMZ::lib/grep.zsh

# HISTORY SUBSTRING SEARCHING
zinit light zsh-users/zsh-history-substring-search
zinit ice wait'0b' lucid atload'bindkey "$terminfo[kcuu1]" history-substring-search-up; bindkey "$terminfo[kcud1]" history-substring-search-down'
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Load url-quote-magic & bracketed-paste-magic.
autoload -U url-quote-magic bracketed-paste-magic
zle -N self-insert url-quote-magic
zle -N bracketed-paste bracketed-paste-magic

### Fix Slowness Of Pastes With `zsh-syntax-highlighting`.
pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic
}
pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

if [[ "$TERM" != "linux" ]]; then
  zinit light zsh-users/zsh-autosuggestions
  zinit light zdharma/fast-syntax-highlighting

  eval "$(starship init zsh)"
elif [[ "$TERM" == "linux" ]]; then
  prompt suse
fi

# end profiling
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
    zprof > ~/.zsh_profile$(date +'%s')
fi
