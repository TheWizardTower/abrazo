#!/usr/bin/env bash

function getGitRepo {
    REPO_URL=$1
    REPO_DIR=$2

    if [ -d "$REPO_DIR" ]
    then
       pushd "$REPO_DIR" || exit
       git pull
       stack install
       popd || exit
    else
           git clone "$REPO_URL"
           pushd "$REPO_DIR" || exit
           stack install
           popd || exit
    fi
}

stack setup
stack update

for ii in \
    cpphs\
    dhall\
    ghc-mod\
    ghcid\
    happy\
    hasktags\
    hlint\
    hoogle\
    idris\
    intero\
    pandoc\
    present\
    ShellCheck\
    stylish-haskell\
    threadscope\
    turtle\
    weeder\
    ; do
    stack install $ii
done

pushd ~/git || exit
getGitRepo "https://github.com/carlohamalainen/ghc-imported-from" "ghc-imported-from"
# Doing it the cave-man way because there's an elisp file in here
# to make this play with Emacs. Stack stores it as a tgz.
getGitRepo "https://github.com/alanz/HaRe.git" "HaRe"
getGitRepo "https://github.com/creichert/stack-tag.git" "stack-tag"
popd || exit
