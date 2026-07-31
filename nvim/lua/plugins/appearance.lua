return {
    -- Visual appearance and UI improvements
    {
        "folke/twilight.nvim",
        event = "BufReadPre",
        opts = {
            dimming = {
                alpha = 0.25,
                color = { "Normal", "#ffffff" },
            },
            context = 10,
            treesitter = true,
            expand = { "function", "method", "table", "if_statement" },
        },
    },
}
