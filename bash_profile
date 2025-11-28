# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH="$PATH:$HOME/.local/bin"
PATH="$PATH:$HOME/local/bin"
PATH="$PATH:$HOME/.local/share/coursier/bin"
PATH="$PATH:$HOME/bin"

export PATH

[ -s "/home/merlin/.jabba/jabba.sh" ] && source "/home/merlin/.jabba/jabba.sh"
if [ -e /home/merlin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/merlin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
. "$HOME/.cargo/env"

. "$HOME/.atuin/bin/env"
