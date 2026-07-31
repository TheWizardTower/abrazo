return {
    -- Trailing whitespace configuration with auto-cleanup on save
    {
        "echasnovski/mini.trailspace",
        version = "*",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            require('config.trailing-space')
        end,
    },
}
