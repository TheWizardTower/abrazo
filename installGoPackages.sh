#!/bin/bash

USER=$(whoami)
HOMEDIR="/home/$USER"

if [ "$USER" = "root" ]; then
   export HOMEDIR="/root/"
fi

export GOPATH="${HOMEDIR}/gocode"

go get -u golang.org/x/tools/cmd/cover
go get -u github.com/kbrgl/fu
sudo -E go get -u golang.org/x/tools/cmd/godoc
go get -u golang.org/x/tools/cmd/gorename
go get -u golang.org/x/tools/cmd/guru
go get -u golang.org/x/tools/cmd/oracle
go get -u golang.org/x/tools/cmd/vet

go get -u github.com/alecthomas/gometalinter
go get -u github.com/motemen/gore
go get -u github.com/nsf/gocode
go get -u github.com/peco/peco/cmd/peco
go get -u github.com/rogpeppe/godef
go get -u github.com/tokozedg/sman

$HOMEDIR/gocode/bin/gometalinter --install --update
