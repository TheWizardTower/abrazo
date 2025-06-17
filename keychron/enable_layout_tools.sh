#!/usr/bin/env bash
set -euo pipefail

# By default, standard users don't have read/write access to the HID raw
# devices. This means that the layout tools QMK dorks love to use will not
# work. Leave this script as a breadcrumb in case I get bamboozled by this in
# the future.
sudo chmod a+rw /dev/hidraw*
