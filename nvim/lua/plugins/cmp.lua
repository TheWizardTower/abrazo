return {
  -- nvim-cmp: Completion engine
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    config = function()
        require('config.cmp')
    end,
    dependencies = {
      -- Snippet engine
      {
        "L3MON4D3/LuaSnip",
        build = "make install_jsregexp",
        dependencies = {
          -- Snippet collection
          "rafamadriz/friendly-snippets",
        },
        config = function()
          require("luasnip.loaders.from_vscode").lazy_load()
        end,
      },
      -- Copilot integration with cmp
      {
        "zbirenbaum/copilot-cmp",
        config = function()
          require("copilot_cmp").setup()
        end,
      },
      -- Completion sources
      "saadparwaiz1/cmp_luasnip",       -- luasnip source
      "hrsh7th/cmp-nvim-lsp",           -- LSP source
      "hrsh7th/cmp-buffer",             -- buffer words
      "hrsh7th/cmp-path",               -- file paths
      "hrsh7th/cmp-nvim-lua",           -- nvim lua API
      "hrsh7th/cmp-cmdline",            -- cmdline completion
      "hrsh7th/cmp-calc",               -- calculator
      "f3fora/cmp-spell",               -- spell checking
      -- Icons for completion menu
      "onsails/lspkind.nvim",
    },
  },
}
