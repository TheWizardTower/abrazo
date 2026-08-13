-- Diagnostics configuration for Neovim LSP

local diagnostic_config = {
    virtual_text = {
        prefix = "●",
        spacing = 2,
    },
    signs = {
        active = {
            untracked = false,
        },
        text = {
            [vim.diagnostic.severity.ERROR] = "✖",
            [vim.diagnostic.severity.WARN] = "▲",
            [vim.diagnostic.severity.HINT] = "⚑",
            [vim.diagnostic.severity.INFO] = "➤",
        },
    },
    underline = true,
    update_in_insert = false,
    severity_sort = true,
}

-- Set global diagnostics configuration
vim.diagnostic.config(diagnostic_config)

-- Create augroup for buffer-local diagnostic settings
local diag_augroup = vim.api.nvim_create_augroup("LspDiagnostics", { clear = true })

-- Configure diagnostics per filetype when buffer is loaded
vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
    group = diag_augroup,
    callback = function()
        local bufnr = vim.api.nvim_get_current_buf()

        -- Add virtual text to diagnostics for better visibility
        vim.diagnostic.enable(true, { namespace_id = 0, bufnr = bufnr })
    end,
})

-- Key mappings for diagnostic navigation
local map = vim.keymap.set

-- Diagnostics navigation
map("n", "[d", function()
    vim.diagnostic.goto_prev({ float = { border = "rounded" } })
end, { desc = "Previous diagnostic" })

map("n", "]d", function()
    vim.diagnostic.goto_next({ float = { border = "rounded" } })
end, { desc = "Next diagnostic" })

-- Show diagnostics in floating window
map("n", "<leader>ld", function()
    vim.diagnostic.open_float({ border = "rounded" })
end, { desc = "Show buffer diagnostics" })

-- Quickfix list with all diagnostics
map("n", "<leader>lq", function()
    vim.diagnostic.setqflist({ title = "LSP Diagnostics" })
end, { desc = "Diagnostics to quickfix list" })

-- Buffer-local diagnostic actions
map("n", "<leader>lx", function()
    vim.diagnostic.reset(vim.api.nvim_get_current_buf())
end, { desc = "Clear buffer diagnostics" })

-- LSP code actions on selection
map({ "n", "x" }, "<leader>a", function()
    vim.lsp.buf.code_action({
        context = {
            diagnostics = vim.diagnostic.get(),
        },
    })
end, { desc = "LSP Code Actions" })

-- Show LSP client info for current buffer
map("n", "<leader>li", function()
    local clients = vim.lsp.get_clients({ bufnr = 0 })
    if #clients == 0 then
        vim.notify("No LSP servers attached to this buffer", vim.log.levels.WARN)
        return
    end

    local msg = {}
    for _, client in ipairs(clients) do
        table.insert(
            msg,
            string.format("• %s (id: %d, capabilities: %s)", client.name, client.id, client.supported_methods())
        )
    end

    vim.ui.list(msg, { prompt = "LSP Clients:" })
end, { desc = "Show LSP clients" })
