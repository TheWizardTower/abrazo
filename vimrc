set nocompatible
filetype off
syntax on

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'flazz/vim-colorschemes'

Plugin 'colepeters/spacemacs-theme.vim'

call vundle#end()
filetype plugin indent on

set background=dark
colorscheme spacemacs-theme
