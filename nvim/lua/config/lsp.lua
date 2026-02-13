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
        ocamllsp = {},
        vtsls = {},
        ts_ls = {},
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
        bashls = {},
        ast_grep = {},
        basedpyright = {},
        biome = {},
        -- nushell = {},
        fish_lsp = {},
        oxlint = {},
        -- starlark = {},
        starlark_rust = {},
        docker_compose_language_service = {},
        docker_language_server = {},
        dockerls = {},
        harper_ls = {},
        just = {},
        eslint = {},

    },
    inlay_hints = {
        enabled = true,
    },
    init_options = {
        lint = {
            enabled = true,
        },
    }
})


-- require("ltex_extra").setup {}
-- vim.lsp.config("latexindent")
-- vim.lsp.config("ltex-ls")
-- vim.lsp.config("ltex-ls-plus")


-- vim.lsp.enable("jq")

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
vim.lsp.enable("docker_compose_language_service")
vim.lsp.enable("docker_language_server")
vim.lsp.enable("dockerls")
vim.lsp.enable("dprint")
vim.lsp.enable("eslint")
vim.lsp.enable("fish_lsp")
vim.lsp.enable("gitlab_ci_ls")
vim.lsp.enable("harper_ls")
vim.lsp.enable("hydra_lsp")
vim.lsp.enable("jqls")
vim.lsp.enable("jsonls")
vim.lsp.enable("just")
vim.lsp.enable("latexindent")
vim.lsp.enable("ltex-ls")
vim.lsp.enable("ltex-ls-plus")
vim.lsp.enable("marksman")
vim.lsp.enable("nushell")
vim.lsp.enable("ocammlsp")
vim.lsp.enable("oxlint")
vim.lsp.enable("proselint")
vim.lsp.enable("quick_lint_js")
vim.lsp.enable("starlark")
vim.lsp.enable("starlark_rust")
vim.lsp.enable("tectonic")
vim.lsp.enable("terraformls")
vim.lsp.enable("tex-fmt")
vim.lsp.enable("texlab")
vim.lsp.enable("tflint")
vim.lsp.enable("ts_ls")
vim.lsp.enable("vale")
vim.lsp.enable("vale-ls")
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


-- Some servers have issues with backup files, see #649
vim.opt.backup = false
vim.opt.writebackup = false

-- Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
-- delays and poor user experience
vim.opt.updatetime = 300

-- Always show the signcolumn, otherwise it would shift the text each time
-- diagnostics appeared/became resolved
vim.opt.signcolumn = "yes"

local keyset = vim.keymap.set
-- Autocomplete
function _G.check_back_space()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end


-- Apply codeAction to the selected region
-- Example: `<leader>aap` for current paragraph
local opts = { silent = true, nowait = true }


