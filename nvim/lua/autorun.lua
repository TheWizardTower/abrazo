vim.g.detectindent_preferred_expandtab = 1
vim.g.detectindent_preferred_indent = 2

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = "*",
  callback = function()
    -- Only run if DetectIndent command is available (plugin may not be installed)
    if vim.fn.exists(":DetectIndent") == 2 then
      vim.cmd("DetectIndent")
    end
  end,
})
