-- Trailing whitespace configuration

-- Enable mini.trailspace for automatic cleanup
local trailspace = require("mini.trailspace")
trailspace.setup({
    -- Highlight trailing spaces in a subtle way
    highlight = {
        "ExtraWhitespace",
    },
})

-- Trim trailing whitespace on save (but not before - let the user see it first)
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    group = vim.api.nvim_create_augroup("TrimTrailingWhitespace", { clear = true }),
    callback = function()
        require("mini.trailspace").trim()
    end,
})

-- Show trailing whitespace visually (uncomment to enable)
vim.api.nvim_set_hl(0, "ExtraWhitespace", {
    ctermbg = "darkred",
    guibg = "#592b2b",
})

