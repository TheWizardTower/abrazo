return {
    {
        "chentoast/marks.nvim",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require("marks").setup({})
        end,
    },
}
