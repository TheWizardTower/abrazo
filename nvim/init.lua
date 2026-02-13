vim.cmd([[
  filetype on
  filetype plugin on
  filetype indent on
  syntax on
  set tabstop=8
  set shiftwidth=2
  set expandtab
  set softtabstop=8

  set list
  set listchars=space:·,trail:·
]])

vim.wo.number = true

require("config.lazy")
require("autorun")

-- Set colorscheme (habamax is built into Neovim)
vim.cmd('colorscheme habamax')


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
