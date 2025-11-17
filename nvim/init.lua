vim.cmd([[
  filetype on
  filetype plugin on
  filetype indent on
  syntax on
  set tabstop=8
  set shiftwidth=2
  set expandtab
  set softtabstop=8
  autocmd BufRead Tiltfile set filetype=bzl
  autocmd BufRead infra-Tiltfile set filetype=bzl


  set list
  set listchars=space:·,trail:·

]])

vim.wo.number = true

require("config.lazy")
require("autorun")
require("config.mason")
require("config.lsp")
require("config.dap")
require("config.cmp")
require("config.telescope")
require("config.which-key")
require("config.conform")
require("config.neogit")
require("config.lualine")
require("config.gitsigns")
require("config.comment")
require("config.autopairs")


-- Set completeopt for nvim-cmp
vim.opt.completeopt = { "menu", "menuone", "noselect" }


-- -- Set the highlight for trailing whitespace
-- vim.api.nvim_set_hl(0, "ExtraWhitespace", { ctermbg = "darkred", bg = "darkred" })
--
-- -- Autocommand to highlight trailing whitespace in all buffers
-- vim.api.nvim_create_autocmd("BufWinEnter", {
--     pattern = "*",
--     callback = function()
--         vim.fn.matchadd("ExtraWhitespace", [[\s+$]])
--     end,
-- })
