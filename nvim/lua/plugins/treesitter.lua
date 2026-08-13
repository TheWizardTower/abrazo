return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      -- nvim-treesitter-context for showing context around cursor
      "nvim-treesitter/nvim-treesitter-context",
    },
    config = function()
      -- Create a deferred setup that runs after the plugin is loaded
      local augroup = vim.api.nvim_create_augroup("TreesitterSetup", { clear = true })
      vim.api.nvim_create_autocmd("User", {
        group = augroup,
        pattern = "LazyLoaded",
        callback = function(event)
          -- Check if this is our plugin
          if event.data and event.data.plugin == "nvim-treesitter" then
            -- Now that the plugin is loaded, run setup
            require('config.treesitter')
            -- Enable incremental selection (requires nvim 0.10+)
            pcall(function()
              require('nvim-treesitter.configs').setup {
                incremental_selection = {
                  enable = true,
                  keymaps = {
                    init_selection = "<C-n>",
                    node_incremental = "<C-n>",
                    scope_incremental = false,
                    node_decremental = "<C-p>",
                  },
                },
              }
            end)
            -- Remove the autocmd after running
            vim.api.nvim_clear_autocmds({ group = augroup })
          end
        end,
      })
    end,
  },
}
