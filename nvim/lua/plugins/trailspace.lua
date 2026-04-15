return {
    {
        "echasnovski/mini.trailspace",
        version = "*",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require("mini.trailspace").setup()
            vim.api.nvim_create_autocmd("BufWritePre", {
                group = vim.api.nvim_create_augroup("MiniTrailspaceTrim", { clear = true }),
                callback = function()
                    require("mini.trailspace").trim()
                end,
            })
        end,
    },
}
