return {
    {
        "nvim-pack/nvim-spectre",
        dependencies = { "nvim-lua/plenary.nvim" },
        cmd = "Spectre",
        keys = {
            { "<leader>sr", function() require("spectre").open() end, desc = "Spectre: search/replace" },
            { "<leader>sw", function() require("spectre").open_visual({ select_word = true }) end, desc = "Spectre: search word" },
            { "<leader>sf", function() require("spectre").open_file_search({ select_word = true }) end, desc = "Spectre: search in file" },
        },
        config = function()
            require("spectre").setup()
        end,
    },
}
