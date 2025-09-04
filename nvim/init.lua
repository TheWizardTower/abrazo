vim.cmd([[
  filetype on
  filetype plugin on
  filetype indent on
  syntax on
  set tabstop=8
  set shiftwidth=4
  set expandtab
  set softtabstop=8
  autocmd BufRead Tiltfile setf=tiltfile
  autocmd BufRead infra-Tiltfile setf=tiltfile
]])

vim.wo.number = true

require("config.lazy")
require("autorun")
require("config.mason")
require("config.formatter")
require("config.lsp")
require("config.telescope")
require("config.which-key")
require("config.conform")
require("config.neogit")


vim.opt.completeopt = { "menuone", "noselect", "popup" }
