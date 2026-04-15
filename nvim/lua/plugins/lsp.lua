return {
  {
    'junnplus/lsp-setup.nvim',
    dependencies = {
      'neovim/nvim-lspconfig',
      'williamboman/mason.nvim',                         -- optional
      'WhoIsSethDaniel/mason-tool-installer.nvim',       -- optional
      'williamboman/mason-lspconfig.nvim',               -- optional
    },
    config = function()
      require('config.lsp')
    end,
    event = { "BufReadPre", "BufNewFile" },
    keys = {
      { '<leader>lf', ':lua vim.lsp.buf.format()<CR>', desc = "Format (LSP)" },
    },
  },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      {
        "SmiteshP/nvim-navbuddy",
        dependencies = {
          "SmiteshP/nvim-navic",
          "MunifTanjim/nui.nvim",
        },
        opts = { lsp = { auto_attach = true } },
      },
    },
  },

  { "junegunn/fzf" },
  { "junegunn/fzf.vim" },
  { "gfanto/fzf-lsp.nvim" },
  { "nvim-lua/plenary.nvim" },
  { "RishabhRD/popfix" },
  { "RishabhRD/nvim-lsputils" },
  { "jubnzv/virtual-types.nvim" },

  {
    "Dan7h3x/signup.nvim",
    branch = "main",
    opts = {},
    config = function(_, opts)
      require("signup").setup(opts)
    end,
  },

  {
    "williamboman/mason.nvim",
    config = function()
      require('config.mason')
    end,
  },

  {
    'nvimdev/lspsaga.nvim',
    config = function()
      require('lspsaga').setup({})
    end,
    dependencies = {
      'nvim-treesitter/nvim-treesitter',       -- optional
      'nvim-tree/nvim-web-devicons',           -- optional
    },
  },
}
