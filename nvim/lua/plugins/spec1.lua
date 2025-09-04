return {


    {
        "zbirenbaum/copilot.lua",
        config = function()
            require("copilot").setup({})
        end,
    },
    {
        "CopilotC-Nvim/CopilotChat.nvim",
        dependencies = {
            { "github/copilot.vim" },                       -- or zbirenbaum/copilot.lua
            { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
        },
        build = "make tiktoken",                            -- Only on MacOS or Linux
        opts = {
            -- See Configuration section for options
        },
        cmd = "Copilot",
        event = "InsertEnter"
        -- See Commands section for default commands if you want to lazy load on them
    },
    { 'gennaro-tedesco/nvim-peekup' },
    -- lazy.nvim
    -- { "/HiPhish/rainbow-delimiters.nvim" },
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        },
        keys = {
            {
                "<leader>?",
                function()
                    require("which-key").show({ global = false })
                end,
                desc = "Buffer Local Keymaps (which-key)",
            },
        },
    },
    -- Lua
    {
        "folke/twilight.nvim",
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        }
    },

    -- Lua
    {
        "folke/twilight.nvim",
        opts = {

            dimming = {
                alpha = 0.25, -- amount of dimming
                -- we try to get the foreground from the highlight groups or fallback color
                color = { "Normal", "#ffffff" },
                term_bg = "#000000", -- if guibg=NONE, this will be used to calculate text color
                inactive = false,    -- when true, other windows will be fully dimmed (unless they contain the same buffer)
            },
            context = 10,            -- amount of lines we will try to show around the current line
            treesitter = true,       -- use treesitter when available for the filetype
            -- treesitter is used to automatically expand the visible text,
            -- but you can further control the types of nodes that should always be fully expanded
            expand = { -- for treesitter, we we always try to expand to the top-most ancestor with these types
                "function",
                "method",
                "table",
                "if_statement",
            },
            exclude = {}, -- exclude these filetypes
        }
    },
    { 'nvim-treesitter/highlight.lua' },
    { 'nvim-treesitter/nvim-treesitter' },
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",  -- required
            "sindrets/diffview.nvim", -- optional - Diff integration

            -- Only one of these is needed.
            "nvim-telescope/telescope.nvim", -- optional
            "ibhagwan/fzf-lua",              -- optional
            "echasnovski/mini.pick",         -- optional
        },
        config = true
    },

    { 'mhartington/formatter.nvim' },
    { 'hrsh7th/cmp-nvim-lsp',
              dependencies = {'hrsh7th/nvim-cmp'},
              event = { "InsertEnter", "CmdlineEnter" }
    },
    { 'onsails/lspkind.nvim' },

    {
        "tris203/precognition.nvim",
        --event = "VeryLazy",
        opts = {
            -- startVisible = true,
            -- showBlankVirtLine = true,
            -- highlightColor = { link = "Comment" },
            -- hints = {
            --      Caret = { text = "^", prio = 2 },
            --      Dollar = { text = "$", prio = 1 },
            --      MatchingPair = { text = "%", prio = 5 },
            --      Zero = { text = "0", prio = 1 },
            --      w = { text = "w", prio = 10 },
            --      b = { text = "b", prio = 9 },
            --      e = { text = "e", prio = 8 },
            --      W = { text = "W", prio = 7 },
            --      B = { text = "B", prio = 6 },
            --      E = { text = "E", prio = 5 },
            -- },
            -- gutterHints = {
            --     G = { text = "G", prio = 10 },
            --     gg = { text = "gg", prio = 9 },
            --     PrevParagraph = { text = "{", prio = 8 },
            --     NextParagraph = { text = "}", prio = 8 },
            -- },
            -- disabled_fts = {
            --     "startify",
            -- },
        },
    },

}
