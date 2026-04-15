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


-- Additional LSP servers enabled via native vim.lsp.enable() (nvim 0.11+).
-- Only servers NOT already configured via lsp-setup above are listed here.
-- Each server is gated on its binary being available (installed directly or via Mason).

-- Helper: check if a binary is available on PATH (includes Mason's bin dir)
local function has_binary(name)
    return vim.fn.executable(name) == 1
end

-- Map of server name -> binary name (or command) to check.
-- Servers already configured via lsp-setup.nvim are NOT listed here to avoid double-starting.
local additional_servers = {
    awk_ls           = "awk-language-server",
    bacon_ls         = "bacon-ls",
    bzl              = "bzl",
    clangd           = "clangd",
    cmake            = "cmake-language-server",
    cspell_ls        = "cspell",
    dprint           = "dprint",
    gitlab_ci_ls     = "gitlab-ci-ls",
    hydra_lsp        = "hydra-lsp",
    jqls             = "jq-lsp",
    jsonls           = "vscode-json-language-server",
    latexindent      = "latexindent",
    ["ltex-ls"]      = "ltex-ls",
    ["ltex-ls-plus"] = "ltex-ls-plus",
    marksman         = "marksman",
    nushell          = "nu",
    ocamllsp         = "ocamllsp",
    proselint        = "proselint",
    quick_lint_js    = "quick-lint-js",
    starlark         = "tilt",
    starlark_rust    = "starlark",
    tectonic         = "tectonic",
    terraformls      = "terraform-ls",
    ["tex-fmt"]      = "tex-fmt",
    texlab           = "texlab",
    tflint           = "tflint",
    vale             = "vale",
    ["vale-ls"]      = "vale-ls",
    yamlls           = "yaml-language-server",
}

for server, binary in pairs(additional_servers) do
    if has_binary(binary) then
        vim.lsp.enable(server)
    end
end


-- require("ltex_extra").setup {}


-- setup() is also available as an alias
require('lspkind').init({
    -- defines how annotations are shown
    -- default: symbol
    -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
    mode = 'symbol_text',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    preset = 'codicons',

    -- override preset symbols
    symbol_map = {
        Text = "󰉿",
        Method = "󰆧",
        Function = "󰊕",
        Constructor = "",
        Field = "󰜢",
        Variable = "󰀫",
        Class = "󰠱",
        Interface = "",
        Module = "",
        Property = "󰜢",
        Unit = "󰑭",
        Value = "󰎠",
        Enum = "",
        Keyword = "󰌋",
        Snippet = "",
        Color = "󰏘",
        File = "󰈙",
        Reference = "󰈇",
        Folder = "󰉋",
        EnumMember = "",
        Constant = "󰏿",
        Struct = "󰙅",
        Event = "",
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
