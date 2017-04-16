if [ -d ~/spacemacs ]
    then
    pushd ~/spacemacs
    git pull
    popd
else
    git clone https://github.com/syl20bnr/spacemacs.git ~/spacemacs.git
fi

ln -sf ~/spacemacs ~/.emacs.d
