# .zshrc

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

### Added by Zinit's installer
declare -A ZINIT
ZINIT[HOME_DIR]=$HOME/.zsh/zinit
ZINIT[ZCOMPDUMP_PATH]=${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump-$ZSH_VERSION

if [[ ! -f ${ZINIT[HOME_DIR]}/bin/zinit.zsh ]]; then
    	print -P "%F{blue}▓▒░ %F{yellow}Installing %F{blue}DHARMA%F{yellow} Initiative Plugin Manager (%F{blue}zdharma/zinit%F{yellow})…%f"
    	mkdir -p "${ZINIT[HOME_DIR]}" && chmod g-rwX "${ZINIT[HOME_DIR]}"
    	git clone https://github.com/zdharma/zinit "${ZINIT[HOME_DIR]}/bin" && {
        	print -P "%F{blue}▓▒░ %F{34}Installation successful.%f%b" ||
        	print -P "%F{red}▓▒░ The clone has failed.%f%b"
	}
fi

source "${ZINIT[HOME_DIR]}/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

## zsh prompt
zinit lucid light-mode for \
	romkatv/powerlevel10k

## PLUGINS
zinit wait lucid light-mode for \
        atinit"
  	  	typeset -gA FAST_HIGHLIGHT
  	  	FAST_HIGHLIGHT[git-cmsg-len]=100
  	  	ZINIT[COMPINIT_OPTS]=-C
  	  	zicompinit
  	  	zicdreplay
  	" \
        	zdharma/fast-syntax-highlighting \
        blockf atpull'zinit creinstall -q .' \
        atinit"
        " \
        atload'
		eval "$(dircolors)"
		zstyle ":completion:*:default" list-colors "${(s.:.)LS_COLORS}" "ma=38;5;7;7;1"
		zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#) ([0-9a-z-]#)*=36=0=01"
  	' \
            zsh-users/zsh-autosuggestions \
        	zsh-users/zsh-completions \
        atinit"
  		ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
  		ZSH_AUTOSUGGEST_STRATEGY=(history completion)
  		ZSH_AUTOSUGGEST_COMPLETION_IGNORE='_*|pre(cmd|exec)|sudo pacman -S*|pacman -S*|paru -S*|yay -S*|\)\*'
  	" \
        	OMZ::lib/git.zsh \
        atload"unalias grv" \
            OMZ::plugins/git/git.plugin.zsh


## zsh settings

zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit promptinit
compinit -i
promptinit

# setopt case_glob              # Use Case-Insensitve Globbing.
setopt globdots                 # Glob Dotfiles As Well.
setopt extendedglob             # Use Extended Globbing.
setopt auto_cd                  # no need to specify cd
setopt correct                  # Turn On Corrections
setopt correct                  # spelling correction
setopt interactivecomments 	    # Ignore lines prefixed with '#'

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
zstyle ':completion::complete:*' cache-path '${ZDOTDIR:-$HOME}/.zcompcache'
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':fzf-tab:*' query-string prefix first
zstyle ':fzf-tab:*' continuous-trigger '/'
zstyle ':completion:*:*:*:*:processes' command'ps -u $USER -o pid,user,comm,cmd -w -w'
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':fzf-tab:*' popup-pad 0 0
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:exa' file-sort modification
zstyle ':completion:*:exa' sort false  	

# history substring search
# HISTORY SUBSTRING SEARCHING
zinit light zsh-users/zsh-history-substring-search
zinit ice wait'0b' lucid atload'bindkey "$terminfo[kcuu1]" history-substring-search-up; bindkey "$terminfo[kcud1]" history-substring-search-down'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='underline'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND=''
zle -N history-substring-search-up
zle -N history-substring-search-down
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down

# History file configuration
HISTFILE="$HOME/.zsh_hist"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000
setopt extended_history          # record timestamp of command in HISTFILE
setopt hist_expire_dups_first    # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups          # Don't record an entry that was just recorded again
setopt hist_find_no_dups         # Do not display a line previously found
setopt hist_ignore_space         # ignore commands that start with space
setopt hist_verify               # show command with history expansion to user before running it
setopt inc_append_history        # add commands to HISTFILE in order of execution
setopt share_history             # shell share history with other tabs

autoload -U add-zsh-hook

# aliases
source "$HOME/.zsh/aliases.zsh"

# better url management
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

## keybindings
bindkey -e                                        # emacs keybindings
bindkey '^[[1;5C' forward-word                    # ctrl + ->
bindkey '^[[1;5D' backward-word                   # ctrl + <-
bindkey '^[[5~' beginning-of-buffer-or-history    # page up
bindkey '^[[6~' end-of-buffer-or-history          # page down
bindkey '^[[H' beginning-of-line                  # home
bindkey '^[[F' end-of-line                        # end


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] && [[ "$TERM" != "linux" ]] || source ~/.p10k.zsh

# eval $(starship init zsh)
