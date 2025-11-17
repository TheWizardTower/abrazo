return {
    {
        'junnplus/lsp-setup.nvim',
        dependencies = {
            'neovim/nvim-lspconfig',
            'williamboman/mason.nvim',                   -- optional
            'WhoIsSethDaniel/mason-tool-installer.nvim', -- optional
            'williamboman/mason-lspconfig.nvim',         -- optional
        },
        keys = {
            { '<leader>f', ':lua vim.lsp.buf.format()' },
        },
    },

    "neovim/nvim-lspconfig",
    dependencies = {
        {
            "SmiteshP/nvim-navbuddy",
            dependencies = {
                "SmiteshP/nvim-navic",
                "nvimdev/lspsaga.nvim",
                "MunifTanjim/nui.nvim"
            },
            opts = { lsp = { auto_attach = true } }
        }
    },
    -- your lsp config or other stuff

    "junegunn/fzf",
    "junegunn/fzf.vim",
    "gfanto/fzf-lsp.nvim",
    "nvim-lua/plenary.nvim",
    'RishabhRD/popfix',
    'RishabhRD/nvim-lsputils',
    'jubnzv/virtual-types.nvim',
    {
        "Dan7h3x/signup.nvim",
        branch = "main",
        opts = {
            -- Your configuration options here
        },
        config = function(_, opts)
            require("signup").setup(opts)
        end
    },
    {
        "williamboman/mason.nvim"
    },
    { "neoclide/coc.nvim" },
    {
        'nvimdev/lspsaga.nvim',
        config = function()
            require('lspsaga').setup({})
        end,
        dependencies = {
            'nvim-treesitter/nvim-treesitter', -- optional
            'nvim-tree/nvim-web-devicons',     -- optional
        },
    }
}
