return {
    {
        "numToStr/Comment.nvim",
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            "nvim-treesitter/nvim-treesitter",
            "JoosepAlviste/nvim-ts-context-commentstring",
        },
    },
}

