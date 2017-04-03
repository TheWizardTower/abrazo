# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


if [[ "$term" -eq "eterm-color" ]]
then
    export TERM="vt100"
fi

SSH_ENV=$HOME/.ssh/environment

function gen_haproxy {
    perl ../../scripts/generate-haproxy-config.pl --pool $1 > haproxy.cfg.${1}.new
    diff -u haproxy.cfg.${1} haproxy.cfg.${1}.new
}

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > ${SSH_ENV}
  echo succeeded
  chmod 600 ${SSH_ENV}
  . ${SSH_ENV} > /dev/null
  /usr/bin/ssh-add ~/.ssh/amccullough-corp-20150911 ~/.ssh/amccullough-prod-20150910 ~/.ssh/github.com-imvu ~/.ssh/github
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

export REPOS="svn+ssh://amccullough@svn.corp.imvu.com/home/svnrepos/trunk/operations/"
export GOPATH="$HOME/gocode:$HOME/code/golang:$HOME/imvu/operations/golang/imvu:$HOME/imvu/operations/golang/external:$GOPATH"
export PERL5LIB="$HOME/imvu/operations/software/ImvuPerlModules"

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

# This only works in Prod, at IMVU. Let's not.
# export PS1=`/usr/local/bin/getps`

alias fix_ssh='ssh-keygen -f "/home/amccullough/.ssh/known_hosts" -R'
alias su_fix_ssh='sudo ssh-keygen -f "/root/.ssh/known_hosts" -R'

function myfix {
    echo $1
    hostname=$1
    ip=`nslookup $hostname | grep Address | grep -v 127.0.0.1 | awk '{ print $2 }'`
    echo $hostname at ip $ip
    fix_ssh $hostname
    fix_ssh $ip
    su_fix_ssh $hostname
    su_fix_ssh $ip
}

alias ia=imvuasset.pl

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"


powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/local/lib/python2.7/dist-packages/powerline/bindings/bash/powerline.sh
