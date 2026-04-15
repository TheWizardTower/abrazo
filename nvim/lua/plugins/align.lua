return {
    {
        "echasnovski/mini.align",
        version = "*",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require("mini.align").setup({
                mappings = {
                    start = "gl",
                    start_with_preview = "gL",
                },
            })
        end,
    },
}
