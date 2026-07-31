
local wk = require("which-key")
wk.add({
    { "<Leader>f",  group = "file" }, -- group
    { "<Leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File", mode = "n" },
    { "<Leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep", mode = "n" },
    { "<Leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers", mode = "n" },
    { "<Leader>fn", desc = "New File" },
    { "<Leader>f1", hidden = true },                                      -- hide this keymap
    { "<Leader>F",  group = "find (FFF)" },                               -- FFF plugin group
    { "<Leader>Ff", function() require("fff").find_files() end, desc = "FFF: find files" },
    { "<Leader>Fg", function() require("fff").live_grep() end, desc = "FFF: live grep" },
    
    { "<Leader>W",  proxy = "<c-w>",                 group = "windows" }, -- proxy to window mappings
    {
        "<Leader>b",
        group = "buffers",
        expand = function()
            return require("which-key.extras").expand.buf()
        end
    },
    {
        -- Nested mappings are allowed and can be added in any order
        -- Most attributes can be inherited or overridden on any level
        -- There's no limit to the depth of nesting
        mode = { "n", "v" },                          -- NORMAL and VISUAL mode
        { "<Leader>q", "<cmd>q<cr>", desc = "Quit" }, -- no need to specify mode since it's inherited
        { "<Leader>w", "<cmd>w<cr>", desc = "Write" },
    },
    
    -- Diagnostic mappings (Category 2 fix)
    {
        "<Leader>d",
        group = "diagnostics",
    },
    { "<Leader>dl", function() vim.diagnostic.setqflist({title="Diagnostics"}) end, desc = "Open diagnostics list" },
    { "<Leader>dn", vim.diagnostic.goto_next, desc = "Next diagnostic" },
    { "<Leader>dp", vim.diagnostic.goto_prev, desc = "Previous diagnostic" },
    { "<Leader>df", function() vim.diagnostic.open_float() end, desc = "Show diagnostics float" },
    { "<Leader>dx", function() vim.diagnostic.reset(vim.api.nvim_get_current_buf()) end, desc = "Clear diagnostics" },
    
    -- LSP mappings (Category 2 fix)
    {
        "<Leader>l",
        group = "LSP",
    },
    { "<Leader>la", vim.lsp.buf.code_action, desc = "Code actions" },
    { "<Leader>lr", vim.lsp.buf.rename, desc = "Rename symbol" },
    { "<Leader>lR", vim.lsp.buf.references, desc = "Find references" },
    { "<Leader>ld", function() vim.diagnostic.open_float() end, desc = "Show diagnostics" },
    { "<Leader>li", function()
        local clients = vim.lsp.get_clients({ bufnr = 0 })
        if #clients == 0 then
            vim.notify("No LSP servers attached to this buffer", vim.log.levels.WARN)
            return
        end
        
        local msg = {}
        for _, client in ipairs(clients) do
            table.insert(msg, string.format(
                "• %s (id: %d)",
                client.name,
                client.id
            ))
        end
        
        vim.ui.list(msg, { prompt = "LSP Clients:" })
    end, desc = "Show LSP clients" },
    
    -- Git mappings (via gitsigns)
    {
        "<Leader>g",
        group = "git",
    },
    { "<Leader>gs", ":Gitsigns stage_hunk<CR>", desc = "Stage hunk", mode = "n" },
    { "<Leader>gr", ":Gitsigns reset_hunk<CR>", desc = "Reset hunk", mode = "n" },
    { "<Leader>gp", ":Gitsigns preview_hunk<CR>", desc = "Preview hunk", mode = "n" },
    { "<Leader>gb", function() require('gitsigns').blame_line() end, desc = "Blame line" },
    
    -- Project/Git mappings
    {
        "<Leader>p",
        group = "project/git",
    },
    { "<Leader>pg", ":Neogit<CR>", desc = "Neogit status" },
    
    -- Jujube (jj) mappings
    {
        "<Leader>j",
        group = "jujube (jj)",
    },
    { "<Leader>js", function() require("config.jujube").open_jj_status() end, desc = "Jujube: status" },
    { "<Leader>jg", function() require("config.jujube").open_jj_log() end, desc = "Jujube: log" },
    { "<Leader>jd", function() require("config.jujube").open_jj_diff() end, desc = "Jujube: diff" },
    
    -- Utility mappings
    {
        "<Leader>u",
        group = "utilities",
    },
    { "<Leader>ut", function() require("twilight").toggle() end, desc = "Toggle twilight dimming" },
    { "<leader>tp", function() require("precognition").toggle() end, desc = "Toggle precognition hints" },
})
