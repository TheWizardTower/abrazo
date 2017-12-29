#!/usr/bin/env bash

if [ -d ~/spacemacs ]
    then
    pushd ~/spacemacs || exit
    git pull
    popd || exit
else
    git clone https://github.com/syl20bnr/spacemacs.git ~/spacemacs
fi

ln -sf ~/spacemacs ~/.emacs.d
