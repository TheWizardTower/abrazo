-- Visual appearance and UI improvements

-- Better cursor line highlighting
vim.wo.cursorline = true
vim.opt.cursorlineopt = "number"

-- Number column settings
vim.wo.number = true
vim.opt.ruler = false

-- Tabline configuration
vim.opt.showtabline = 2

-- Better window split behavior
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Fold column for code folding (ufo.nvim)
vim.o.foldcolumn = "1"
