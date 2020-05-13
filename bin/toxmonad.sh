#!/usr/bin/env bash

kwin_pid=$(pgrep kwin)

kill -9 "${kwin_pid}" && xmonad --replace
