return {
    -- LSP diagnostics configuration with navigation and display improvements
    {
        "nvim-lspconfig/nvim-lspconfig",
        dependencies = { "nvim-telescope/telescope.nvim" },
        config = function()
            require('config.diagnostics')
        end,
    },
}
