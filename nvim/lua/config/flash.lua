require("flash").setup({
  -- Flash enhances the built-in search functionality
  modes = {
    -- Options used when flash is activated through
    -- a regular search with `/` or `?`
    search = {
      enabled = true,
    },
    -- Options used when flash is activated through
    -- `f`, `F`, `t`, `T`, `;` and `,` motions
    char = {
      enabled = true,
      -- Hide after jump when not moving
      autohide = false,
      -- Show jump labels
      jump_labels = true,
      -- Set to `false` to use the current line only
      multi_line = true,
    },
    -- Options used for treesitter selections
    treesitter = {
      enabled = true,
      labels = "abcdefghijklmnopqrstuvwxyz",
      jump = { pos = "range" },
    },
  },
  -- Options for the highlight groups
  highlight = {
    -- Show a backdrop
    backdrop = true,
    -- Highlight the matches
    matches = true,
    -- Highlight the group label
    groups = {
      match = "FlashMatch",
      current = "FlashCurrent",
      backdrop = "FlashBackdrop",
      label = "FlashLabel",
    },
  },
})

-- Set up keymaps
local map = vim.keymap.set

-- Jump to any visible text with flash
map({ "n", "x", "o" }, "s", function()
  require("flash").jump()
end, { desc = "Flash" })

-- Jump to any visible text (backwards)
map({ "n", "x", "o" }, "S", function()
  require("flash").jump({ search = { forward = false } })
end, { desc = "Flash backwards" })

-- Treesitter selection
map({ "n", "o", "x" }, "<leader>v", function()
  require("flash").treesitter()
end, { desc = "Flash Treesitter" })

-- Remote flash - operate on a distant location
map({ "o" }, "r", function()
  require("flash").remote()
end, { desc = "Remote Flash" })

-- Treesitter search - use treesitter to enhance search
map({ "c" }, "<c-s>", function()
  require("flash").toggle()
end, { desc = "Toggle Flash Search" })
