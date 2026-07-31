-- Performance optimization settings for Neovim

-- Reduce LSP trigger delay for faster completion and diagnostics
vim.opt.updatetime = 200  -- Lower than default 300ms

-- Buffer optimization - defer loading of large files
local max_filesize_mb = 5
vim.g.bigfile_size = max_filesize_mb * 1024 * 1024

-- Performance settings for treesitter (in config/treesitter.lua)
vim.g.nvim_treesitter_max_autocommand_line_count = 100

-- Disable unnecessary features on startup
vim.opt.lazyredraw = true    -- Faster execution of scripts
vim.opt.synmaxcol = 120      -- Syntax highlighting max column

-- LSP performance optimizations
local lsp_opts = {
    max_buffer_size = 1 * 1024 * 1024,     -- 1MB buffer for LSP messages
    completion_delay = 100,                 -- Slightly delay completion trigger
    hover_preview = "always",              -- Always show preview in hover
}

-- Set timeout for LSP operations to prevent hanging
vim.opt.timeoutlen = 300   -- Reduced from default 1000ms

-- Optimize scrolling and redraws
vim.opt.lazyredraw = true  -- Skip rendering for register-only commands

-- Reduce flicker during buffer switching
vim.opt.switchbuf = "useopen,utime"  -- Use already open buffers without creating new windows

