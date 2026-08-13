require("telescope").setup({
    defaults = {
        -- Default configuration for telescope goes here:
        mappings = {
            i = {
                -- map actions.which_key to <C-h> (default: <C-/>)
                -- actions.which_key shows the mappings for your picker,
                -- e.g., git_{create, delete, ...}_branch for the git_branches picker
                ["<C-h>"] = "which_key",
            },
        },
        -- Better defaults for file finding
        file_ignore_patterns = { ".git/", "node_modules/" },
        shorten_path = true,
        winblend = 0,
        -- Layout settings
        layout_config = {
            horizontal = {
                prompt_position = "top",
                preview_cutoff = 100,
            },
            vertical = {
                mirror = false,
            },
        },
        -- Sorting and matching
        sorting_strategy = "ascending",
        scroll_strategy = "limit",
        -- Color scheme integration
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
    },
    pickers = {
        -- Default configuration for builtin pickers goes here:
        find_files = {
            hidden = true,
        },
        git_status = {
            show_ignored_files = false,
        },
        -- Project picker with better defaults
        projects = {
            theme = "dropdown",
            previewer = false,
        },
    },
    extensions = {
        -- Live grep args extension
        live_grep_args = {
            auto_quoting = true,
        },
    },
})
