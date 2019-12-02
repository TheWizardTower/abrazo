#!/bin/bash

emacs --eval '(let ((display-buffer-alist `(("^\\*magit: " display-buffer-same-window) ,display-buffer-alist))) (magit-status))'
