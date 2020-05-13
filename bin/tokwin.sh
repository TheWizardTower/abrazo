#!/usr/bin/env bash

xmonad_pid=$(pgrep xmonad)

kill -9 "${xmonad_pid}" && kwin --replace
