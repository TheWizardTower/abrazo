-- Jujube ("jj") configuration for Neovim

local function open_jj_status()
    -- Use :! to run jj status and capture output
    vim.cmd("silent !jj status 2>&1 | head -50")
end

local function open_jj_log()
    vim.cmd("silent !jj log --oneline -20 2>&1 | head -30")
end

local function open_jj_diff()
    vim.cmd("silent !jj diff 2>&1 | head -50")
end

-- Keymaps for Jujube operations
local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { silent = true, desc = desc })
end

-- JJ status picker (shows working copy status)
map("n", "<leader>js", open_jj_status, "Jujube: status")

-- JJ log browser (view commit history)
map("n", "<leader>jg", open_jj_log, "Jujube: log")

-- JJ diff viewer
map("n", "<leader>jd", open_jj_diff, "Jujube: diff")

print("✓ Jujube integration loaded")
