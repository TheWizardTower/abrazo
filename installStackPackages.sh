#!/bin/bash

function getGitRepo {
    REPO_URL=$1
    REPO_DIR=$2

    if [ -d "$REPO_DIR" ]
    then
       pushd "$REPO_DIR"
       git pull
       stack install
       popd
    else
           git clone "$REPO_URL"
           pushd "$REPO_DIR"
           stack install
           popd
    fi
}

stack setup
stack update
stack install ghc-mod
stack install happy
stack install hasktags
stack install hindent
stack install hlint
stack install hoogle
stack install intero
stack install present
stack install ShellCheck
stack install stylish-haskell

pushd ~/git
getGitRepo "https://github.com/carlohamalainen/ghc-imported-from" "ghc-imported-from"
# Doing it the cave-man way because there's an elisp file in here
# to make this play with Emacs. Stack stores it as a tgz.
getGitRepo "https://github.com/alanz/HaRe.git" "HaRe"
getGitRepo "https://github.com/creichert/stack-tag.git" "stack-tag"
popd
