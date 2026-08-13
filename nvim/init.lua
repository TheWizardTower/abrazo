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

-- Safe settings for disk and crash recovery
vim.opt.swapfile = true          -- Keep swap files for crash recovery
vim.opt.backup = false           -- No backup files (we use undo instead)
vim.opt.writebackup = true       -- Write backup during writes
vim.opt.undofile = true          -- Persistent undo
vim.opt.hidden = true            -- Allow buffer switching without save

-- Better updatetime for LSP responsiveness
vim.opt.updatetime = 200         -- Lower than default 400ms for faster diagnostics

-- Signcolumn setup for better visibility of signs (diagnostics, git, etc.)
vim.opt.signcolumn = "yes:2"     -- Always show sign column with 2 characters width

-- Line length indicator
vim.opt.colorcolumn = "80"       -- Show line length guide at 80 chars

-- Completion menu height
vim.opt.pumheight = 15           -- Limit completion menu to 15 items max

-- Set completeopt for nvim-cmp
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Briefly highlight yanked text (evil-goggles equivalent)
vim.api.nvim_create_autocmd("TextYankPost", {
    group = vim.api.nvim_create_augroup("HighlightYank", { clear = true }),
    callback = function()
        vim.highlight.on_yank({ higroup = "IncSearch", timeout = 150 })
    end,
})

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

