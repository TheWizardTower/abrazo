-- Luacheck configuration for Neovim Lua config

-- Allow global variables that are expected in Neovim context
globals = {
    "vim",
    "unpack",  -- Lua 5.1 compatibility
}

-- Ignore linting warnings (not errors)
unused_args = false
unused_secondaries = false

-- Allow reading unused variables (for configuration tables)
read_only_globals = {
    "_G",
}

-- Maximum line length (Neovim style typically allows longer lines for readability)
max_line_length = 120

-- Ignore files that are third-party or auto-generated
files = {
    -- All lua files in nvim/
    "**/*.lua",
}

-- Exclude lazy.nvim since it's a third-party dependency
exclude = {
    "lazy-lock.lua",
}

-- Allow unused variables with _ prefix (convention for intentionally unused)
unused_args_ignore_unused_underscore = true

