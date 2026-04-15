return {
    {
        "dmtrKovalenko/fff.nvim",
        build = function()
            require("fff.download").download_or_build_binary()
        end,
        lazy = false,
        opts = {},
        keys = {
            {
                "<leader>ff",
                function() require("fff").find_files() end,
                desc = "FFF: find files",
            },
            {
                "<leader>fg",
                function() require("fff").live_grep() end,
                desc = "FFF: live grep",
            },
            {
                "<leader>fc",
                function() require("fff").live_grep({ query = vim.fn.expand("<cword>") }) end,
                desc = "FFF: grep word under cursor",
            },
        },
    },
}
