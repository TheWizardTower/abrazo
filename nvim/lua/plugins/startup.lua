return {
    -- Startup time optimization settings
    {
        "dstein64/vim-startuptime",
        event = "VeryLazy",
        init = function()
            vim.g.startuptime_tries = 10
        end,
    },
}
