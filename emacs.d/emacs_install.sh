#!/usr/bin/env bash

set -euo pipefail

# if [ -d emacs-bedrock ]; then
# 	pushd emacs-bedrock
# 	git pull
# 	popd
# else
# 	git clone https://git.sr.ht/~ashton314/emacs-bedrock
# fi
#

if [ -L "${HOME}/.emacs.d" ]; then
	# happy case. continue.
	true
	echo "Link already established."
elif [ -d "${HOME}/.emacs.d" ]; then
	EPOCH_TIME="$(date +%s)"
	mv "${HOME}/.emacs.d" "${HOME}/.emacs.d_${EPOCH_TIME}"
	ln -s "${HOME}/abrazo/emacs.d" "${HOME}/.emacs.d"
fi
