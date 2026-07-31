return {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = {
        'nvim-lua/plenary.nvim',
        'nvim-telescope/telescope-live-grep-args.nvim',
        -- Additional useful extensions
        { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },  -- faster fuzzy finding
    },
    config = function()
        require('config.telescope')
        
        -- Load extensions
        require('telescope').load_extension('live_grep_args')
        
        -- Try to load fzf-native if available (for faster sorting)
        pcall(function()
            require('telescope').load_extension('fzf')
        end)
    end,
}
