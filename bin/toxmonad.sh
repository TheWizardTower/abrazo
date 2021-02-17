#!/usr/bin/env bash

kwin_pid=$(pgrep kwin)

kill -9 "${kwin_pid}" && /home/merlin/.xmonad/xmonad-x86_64-linux --replace
