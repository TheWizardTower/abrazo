# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin

export PATH

export PATH="$HOME/.cargo/bin:$PATH"
[ -s "/home/merlin/.jabba/jabba.sh" ] && source "/home/merlin/.jabba/jabba.sh"
if [ -e /home/merlin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/merlin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
