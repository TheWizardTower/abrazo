require("precognition").setup({
  startVisible = false,  -- Disable by default
  showBlankVirtLine = false,
  highlightColor = { link = "Comment" },
  hints = {
    Caret = { text = "^", prio = 2 },
    Dollar = { text = "$", prio = 1 },
    MatchingPair = { text = "%", prio = 5 },
  },
  gutterHints = {},  -- Disable gutter hints
})

-- Toggle precognition on/off
vim.keymap.set("n", "<leader>tp", function()
  require("precognition").toggle()
end, { desc = "Toggle Precognition hints" })
