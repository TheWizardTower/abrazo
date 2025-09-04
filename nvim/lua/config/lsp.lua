require('lsp-setup').setup({
    on_attach = function(client, bufnr)
        vim.lsp.completion.enable(true, client.id, bufnr, {
            autotrigger = true,
            convert = function(item)
                return { abbr = item.label:gsub("%b()", "") }
            end,
        })
        vim.keymap.set("i", "<C-space>", vim.lsp.completion.get, { desc = "trigger autocompletion" })
        vim.keymap.set("i", "<C-S-space>", vim.lsp.completion.get, { desc = "trigger autocompletion" })
    end,
    servers = {
        lua_ls = {
            settings = {
                Lua = {
                    hint = {
                        enable = true,
                        arrayIndex = "Auto",
                        await = true,
                        paramName = "All",
                        paramType = true,
                        semicolon = "SameLine",
                        setType = false,
                    },
                },
            },
        },
        rust_analyzer = {
            settings = {
                settings = {
                    ['rust-analyzer'] = {
                        inlayHints = {
                            bindingModeHints = {
                                enable = false,
                            },
                            chainingHints = {
                                enable = true,
                            },
                            closingBraceHints = {
                                enable = true,
                                minLines = 25,
                            },
                            closureReturnTypeHints = {
                                enable = 'never',
                            },
                            lifetimeElisionHints = {
                                enable = 'never',
                                useParameterNames = false,
                            },
                            maxLength = 25,
                            parameterHints = {
                                enable = true,
                            },
                            reborrowHints = {
                                enable = 'never',
                            },
                            renderColons = true,
                            typeHints = {
                                enable = true,
                                hideClosureInitialization = false,
                                hideNamedConstructor = false,
                            },
                        }
                    },
                }
            }
        },

    },
    inlay_hints = {
        enabled = true,
    }
})



vim.lsp.enable("ast_grep")
vim.lsp.enable("awk_ls")
vim.lsp.enable("bacon_ls")
vim.lsp.enable("basedpyright")
vim.lsp.enable("bashls")
vim.lsp.enable("biome")
vim.lsp.enable("bzl") -- bazel, which is the origin for starlark.
vim.lsp.enable("clangd")
vim.lsp.enable("cmake")
vim.lsp.enable("cspell_ls")
vim.lsp.enable("deno")
vim.lsp.enable("docker_compose_language_service")
vim.lsp.enable("docker_language_server")
vim.lsp.enable("dockerls")
vim.lsp.enable("dprint")
vim.lsp.enable("eslint")
vim.lsp.enable("fish_lsp")
vim.lsp.enable("gitlab_ci_ls")
vim.lsp.enable("harper_ls")
vim.lsp.enable("hydra_lsp")
vim.lsp.enable("jsonls")
vim.lsp.enable("just")
vim.lsp.enable("marksman")
vim.lsp.enable("nushell")
vim.lsp.enable("oxlint")
vim.lsp.enable("quick_lint_js")
vim.lsp.enable("starlark")
vim.lsp.enable("starlark_rust")
vim.lsp.enable("terraformls")
vim.lsp.enable("tflint")
vim.lsp.enable("ts_ls")
vim.lsp.enable("vtsls")
vim.lsp.enable("yamlls")


-- setup() is also available as an alias
require('lspkind').init({
    -- DEPRECATED (use mode instead): enables text annotations
    --
    -- default: true
    -- with_text = true,

    -- defines how annotations are shown
    -- default: symbol
    -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
    mode = 'symbol_text',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
        Text = "󰉿",
        Method = "󰆧",
        Function = "󰊕",
        Constructor = "",
        Field = "󰜢",
        Variable = "󰀫",
        Class = "󰠱",
        Interface = "",
        Module = "",
        Property = "󰜢",
        Unit = "󰑭",
        Value = "󰎠",
        Enum = "",
        Keyword = "󰌋",
        Snippet = "",
        Color = "󰏘",
        File = "󰈙",
        Reference = "󰈇",
        Folder = "󰉋",
        EnumMember = "",
        Constant = "󰏿",
        Struct = "󰙅",
        Event = "",
        Operator = "󰆕",
        TypeParameter = "",
    },
})

-- local cmp = require 'cmp'
-- cmp.setup({
--     snippet = {
--         expand = function(args)
--             vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
--         end,
--     },
--     window = {
--         completion = cmp.config.window.bordered(),
--         documentation = cmp.config.window.bordered(),
--     },
--     mapping = cmp.mapping.preset.insert({
--         ['<C-b>'] = cmp.mapping.scroll_docs(-4),
--         ['<C-f>'] = cmp.mapping.scroll_docs(4),
--         ['<C-Space>'] = cmp.mapping.complete(),
--         ['<C-e>'] = cmp.mapping.abort(),
--         ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
--     }),
--     sources = cmp.config.sources({
--         { name = 'nvim_lsp' },
--     }, {
--         { name = 'buffer' },
--     })
-- })
