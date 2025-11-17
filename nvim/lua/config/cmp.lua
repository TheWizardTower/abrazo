local cmp = require("cmp")
local luasnip = require("luasnip")
local lspkind = require("lspkind")

-- Helper function for super tab behavior
local has_words_before = function()
    unpack = unpack or table.unpack
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
    -- Enable snippet support
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },

    -- Window appearance
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },

    -- Completion sources (in priority order)
    sources = cmp.config.sources({
        { name = "nvim_lsp", priority = 1000 },  -- LSP completions (highest priority)
        { name = "luasnip", priority = 750 },    -- Snippets
        { name = "nvim_lua", priority = 650 },   -- Neovim Lua API
    }, {
        { name = "buffer", priority = 500 },     -- Buffer words
        { name = "path", priority = 250 },       -- File paths
        { name = "calc", priority = 100 },       -- Calculator
        { name = "spell", priority = 50 },       -- Spell checking
    }),

    -- Key mappings
    mapping = cmp.mapping.preset.insert({
        -- Navigate completion menu
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),

        -- Accept completion
        ["<CR>"] = cmp.mapping.confirm({ select = true }),

        -- Super Tab: Tab to select, Shift+Tab to go back
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),

        -- Navigate snippet placeholders
        ["<C-j>"] = cmp.mapping(function()
            if luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            end
        end, { "i", "s" }),

        ["<C-k>"] = cmp.mapping(function()
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            end
        end, { "i", "s" }),
    }),

    -- Formatting appearance
    formatting = {
        format = lspkind.cmp_format({
            mode = "symbol_text",
            maxwidth = 50,
            ellipsis_char = "...",
            before = function(entry, vim_item)
                return vim_item
            end,
        }),
    },

    -- Performance settings
    performance = {
        debounce = 60,
        throttle = 30,
        fetching_timeout = 500,
    },

    -- Experimental features
    experimental = {
        ghost_text = {
            hl_group = "CmpGhostText",
        },
    },
})

-- Command line completion
cmp.setup.cmdline({ "/", "?" }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = "buffer" },
    },
})

cmp.setup.cmdline(":", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = "path" },
    }, {
        { name = "cmdline" },
    }),
})

-- Setup luasnip keybindings
vim.keymap.set({ "i", "s" }, "<C-l>", function()
    if luasnip.choice_active() then
        luasnip.change_choice(1)
    end
end, { desc = "Change snippet choice" })

