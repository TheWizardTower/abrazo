# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

_keychain_keys=()
for _k in github golem gitlab-home-lab-merlin turkishDelight; do
  [ -f "$HOME/.ssh/$_k" ] && _keychain_keys+=("$_k")
done
[ ${#_keychain_keys[@]} -gt 0 ] && eval "$(keychain --eval --quiet "${_keychain_keys[@]}")"
unset _keychain_keys _k

export EDITOR=nvim

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=50000
HISTFILESIZE=100000
export HISTTIMEFORMAT="%F %T "

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
# [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'

fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

source ~/.shellrc
source ~/.alias
source ~/.alias.sh

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"

[ -s "/home/merlin/.jabba/jabba.sh" ] && source "/home/merlin/.jabba/jabba.sh"

# . ~/.nix-profile/etc/profile.d/nix.sh
# source ~/git/git-subrepo/.rc

[ "$TERM" = "linux" ] && source $HOME/abrazo/tty-colors.sh

export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

source ~/.local/share/blesh/ble.sh
source ~/.bash-powerline.sh

source <(leadr --bash)

[[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh
eval "$(flox activate --trust --dir ~/)"
eval "$(atuin init bash)"

# pnpm
export PNPM_HOME="/home/merlin/.local/share/pnpm"
case ":$PATH:" in
*":$PNPM_HOME:"*) ;;
*) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# Atuin history with fzf integration for search, standard arrow keys for browsing
# Arrow keys: regular bash history (with prefix matching via readline)
bind '"\e[A": history-search-backward' 2>/dev/null || true
bind '"\e[B": history-search-forward' 2>/dev/null || true

# Alt+R: Launch atuin + fzf for powerful search with preview
atuin-fzf() {
  local cmd=$(atuin search --shell-upward-binding-command | fzf --height=40% --layout=reverse --border | sed 's/.*→ //')
  if [ -n "$cmd" ]; then
    READLINE_LINE="$cmd"
    READLINE_POINT=${#READLINE_LINE}
  fi
}

bind -x '"\e[r": atuin-fzf' 2>/dev/null || true

eval "$(atuin init bash --disable-up-arrow)"

source /home/merlin/.config/broot/launcher/bash/br
