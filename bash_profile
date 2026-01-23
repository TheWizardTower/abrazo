
# User specific environment and startup programs
export BUN_INSTALL="${HOME}/.bun"

PATH="${HOME}/.local/bin:${PATH}"
PATH="${PATH}:$HOME/local/bin"
PATH="${PATH}:$HOME/.local/share/coursier/bin"
PATH="${PATH}:$HOME/bin"
PATH="${PATH}:${HOME}/.opencode/bin"
PATH="${BUN_INSTALL}/bin:${PATH}"

export PATH

source ~/.api_keys.sh

[ -s "/home/merlin/.jabba/jabba.sh" ] && source "/home/merlin/.jabba/jabba.sh"
if [ -e /home/merlin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/merlin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
. "$HOME/.cargo/env"

. "$HOME/.atuin/bin/env"

# Set up the $PATH first, then invoke bashrc.

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
