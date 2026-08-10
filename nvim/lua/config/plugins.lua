-- Plugin management configuration for lazy.nvim
-- This file defines plugin groups and provides helpers for lazy.nvim import

local M = {}

-- ==============================================================================
-- Plugin groups organized by category
-- ==============================================================================
M.plugin_groups = {
    -- CORE: Essential plugins required for other plugins to function
    core = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        { "folke/noice.nvim", event = "VeryLazy" },
    },

    -- EDITORS: Text manipulation and editing utilities
    editors = {
        { "numToStr/Comment.nvim" },
        { "kylechui/nvim-surround", version = "*" },
        { "andymass/vim-matchup" },
    },

    -- NAVIGATION: File finding, buffer navigation, and code folding
    navigation = {
        { "nvim-telescope/telescope.nvim", tag = "0.1.8" },
        { "dmtrKovalenko/fff.nvim", cmd = "FFF" },
        { "nvim-telescope/telescope-live-grep-args.nvim" },
        { "kevinhwang91/nvim-ufo", event = { "BufReadPre", "BufNewFile" } },
    },

    -- CODING: AI assistance and coding companions
    coding = {
        { "zbirenbaum/copilot.lua", event = "InsertEnter" },
        { "CopilotC-Nvim/CopilotChat.nvim", event = "InsertEnter" },
        { "olimorris/codecompanion.nvim", cmd = "CodeCompanion" },
    },

    -- LANGUAGE: LSP, completion, and syntax highlighting
    language = {
        { "junnplus/lsp-setup.nvim", event = { "BufReadPre", "BufNewFile" } },
        { "neovim/nvim-lspconfig" },
        { "hrsh7th/nvim-cmp", event = "InsertEnter" },
        { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
        { "nvim-treesitter/nvim-treesitter-textobjects" },
        { "nvim-treesitter/nvim-treesitter-context" },
    },

    -- VERSION CONTROL: Git integration
    version_control = {
        { "lewis6991/gitsigns.nvim", event = { "BufReadPre", "BufNewFile" } },
        { "NeogitOrg/neogit", cmd = "Neogit" },
    },

    -- UI: Statusline, themes, and visual enhancements
    ui = {
        { "nvim-lualine/lualine.nvim" },
        { "folke/twilight.nvim" },
        { "echasnovski/mini.trailspace", version = "*" },
    },
}

-- ==============================================================================
-- Lazy.nvim import helpers
-- ==============================================================================

-- Create a plugin spec for lazy.nvim import
-- Converts a plugin group definition into lazy.nvim import statements
function M.create_plugin_spec(group_name)
    local plugins = M.plugin_groups[group_name]
    if not plugins then
        return {}
    end

    local spec = {}
    for _, plugin in ipairs(plugins) do
        if type(plugin) == "string" then
            -- Simple string spec: "user/repo"
            local name = plugin:gsub(".*/", "")
            table.insert(spec, { import = "plugins." .. name:gsub("%.lua$", "") })
        else
            -- Table spec: { "user/repo", options }
            local name = plugin[1]:gsub(".*/", "")
            table.insert(spec, {
                import = "plugins." .. name:gsub("%.lua$", ""),
                opts = plugin[2]
            })
        end
    end

    return spec
end

-- Get all available plugin group names
function M.get_group_names()
    local names = {}
    for name in pairs(M.plugin_groups) do
        table.insert(names, name)
    end
    table.sort(names)
    return names
end

-- Get all plugins from a specific group
function M.get_plugins(group_name)
    return M.plugin_groups[group_name] or {}
end

return M
