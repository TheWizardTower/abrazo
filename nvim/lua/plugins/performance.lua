return {
    -- Performance optimization settings for Neovim
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            presets = {
                lsp_doc_border = true,
            },
        },
    },
}
