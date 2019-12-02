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
  /usr/bin/ssh-add ~/.ssh/github ~/.ssh/bitbucket
}

# Source SSH settings, if applicable

hostname=$(hostname)
if [ ${hostname} != "lint-sandbox" ]; then
    if [ -f "${SSH_ENV}" ]; then
        . ${SSH_ENV} > /dev/null
        ps ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
    else
        start_agent;
    fi
fi

export GOPATH="$HOME/gocode:$HOME/code/golang:$GOPATH"

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
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias lp='ls --color=yes | less -R'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias ..='cd ..'
source ~/.shellrc
source ~/.alias

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

. /usr/local/lib/python3.7/site-packages/powerline/bindings/bash/powerline.sh
