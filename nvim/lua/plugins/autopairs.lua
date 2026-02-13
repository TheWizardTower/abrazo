return {
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        dependencies = {
            -- Smooth integration with nvim-cmp confirmations
            "hrsh7th/nvim-cmp",
        },
        config = function()
            require('config.autopairs')
        end,
    },
}

