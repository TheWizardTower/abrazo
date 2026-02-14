return {
  -- Preview nvim registers.
  { 'gennaro-tedesco/nvim-peekup' },
  -- Show possible keybindings in popup.
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

  { 'nvim-treesitter/highlight.lua' },
  { 'nvim-treesitter/nvim-treesitter' },

  {
    'hrsh7th/cmp-nvim-lsp',
    dependencies = { 'hrsh7th/nvim-cmp' },
    event = { "InsertEnter", "CmdlineEnter" }
  },
  { 'onsails/lspkind.nvim' },
}
