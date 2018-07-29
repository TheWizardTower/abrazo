#!/usr/bin/env bash

for ii in \
    cargo-check\
        cargo-edit\
        cargo-find\
        cargo-graph\
        cargo-script\
        cargo-update\
        cargo-watch\
        ptags\
        kickstart\
        racer\
        ripgrep\
        rustfmt\
        xsv\
    ; do
    cargo install $ii
done



true
