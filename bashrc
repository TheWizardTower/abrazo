# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

SSH_ENV=$HOME/.ssh/environment

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > ${SSH_ENV}
  echo succeeded
  chmod 600 ${SSH_ENV}
  . ${SSH_ENV} > /dev/null
  /usr/bin/ssh-add ~/.ssh/{aws,github}
  /usr/bin/ssh-add ~/.ssh/merlin*
}

# Source SSH settings, if applicable

hostname=$(hostname)
if [ -f "${SSH_ENV}" ]; then
    . ${SSH_ENV} > /dev/null
    ps ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_agent;
}
else
    start_agent;
fi

export GOPATH="$HOME/gocode:$HOME/code/golang:$GOPATH"
export EDITOR=nvim

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

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

function mymytop() {
    echo $0
    echo $1
    if [ "$1" != "" ]; then
        if ( echo $1 | grep -qiE '^AF00' ); then
            mytop -u$(imvucredentials DB_PS_LIST_USER) -p$(imvucredentials DB_PS_LIST_PASSWORD) -s 1 -h $1
        fi
        if ( echo $1 | grep -qE '^[0-9]{4}$' ); then
            mytop -u$(imvucredentials DB_PS_LIST_USER) -p$(imvucredentials DB_PS_LIST_PASSWORD) -s 1 -h AF00$1
        fi
    else
        echo "ABORT: needs a hostname as parameter. ie: mymytop AF001478"
    fi
}

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"


export PATH="$PATH:$HOME/local/bin"

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1

# . /usr/local/lib/python3.8/site-packages/powerline/bindings/bash/powerline.sh

[ -s "/home/merlin/.jabba/jabba.sh" ] && source "/home/merlin/.jabba/jabba.sh"

# . ~/.nix-profile/etc/profile.d/nix.sh
# source ~/git/git-subrepo/.rc

GPG_TTY=$(tty)
export GPG_TTY
gpgconf --create-socketdir
export PATH="$PATH:$HOME/.local/bin"
eval "$(direnv hook bash)"
. "$HOME/.cargo/env"
eval "$(mcfly init bash)"

if [ "$TERM" = "linux" ]; then
    echo -en "\e]P0222222" #black
    echo -en "\e]P8222222" #darkgrey
    echo -en "\e]P1803232" #darkred
    echo -en "\e]P9982b2b" #red
    echo -en "\e]P25b762f" #darkgreen
    echo -en "\e]PA89b83f" #green
    echo -en "\e]P3aa9943" #brown
    echo -en "\e]PBefef60" #yellow
    echo -en "\e]P4324c80" #darkblue
    echo -en "\e]PC2b4f98" #blue
    echo -en "\e]P5706c9a" #darkmagenta
    echo -en "\e]PD826ab1" #magenta
    echo -en "\e]P692b19e" #darkcyan
    echo -en "\e]PEa1cdcd" #cyan
    echo -en "\e]P7ffffff" #lightgrey
    echo -en "\e]PFdedede" #white
    clear #for background artifacting
fi

export LANG="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

source ~/.bash-powerline.sh
