require("config.lazy")
require("mason").setup()

require('lsp-setup').setup({
	servers = {
		lua_ls = {
			settings = {
				Lua = {
					hint = {
						enable = false,
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
		marksman = {},
		basedpyright = {},
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
		terraformls = {},
		tflint = {},
		harper_ls = {},
		hydra_lsp = {},
	},
	inlay_hints = {
		enabled = true,
	}
})

require("CopilotChat").setup {
	-- See Configuration section for options
}

-- require('bad-practices.nvim').setup({
--     most_splits = 3, -- how many splits are considered a good practice(default: 3)
--     most_tabs = 3, -- how many tabs are considered a good practice(default: 3)
--     max_hjkl = 10, -- how many times you can spam hjkl keys in a row(default: 10)
-- })

require('bad_practices').setup({
	most_splits = 3, -- how many splits are considered a good practice(default: 3)
	most_tabs = 3, -- how many tabs are considered a good practice(default: 3)
	max_hjkl = 10, -- how many times you can spam hjkl keys in a row(default: 10)
})

require("hardtime").setup()


local wk = require("which-key")
wk.add({
	{ "<leader>f",  group = "file" }, -- group
	{ "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File", mode = "n" },
	{ "<leader>fb", function() print("hello") end,   desc = "Foobar" },
	{ "<leader>fn", desc = "New File" },
	{ "<leader>f1", hidden = true },                               -- hide this keymap
	{ "<leader>w",  proxy = "<c-w>",                 group = "windows" }, -- proxy to window mappings
	{
		"<leader>b",
		group = "buffers",
		expand = function()
			return require("which-key.extras").expand.buf()
		end
	},
	{
		-- Nested mappings are allowed and can be added in any order
		-- Most attributes can be inherited or overridden on any level
		-- There's no limit to the depth of nesting
		mode = { "n", "v" },            -- NORMAL and VISUAL mode
		{ "<leader>q", "<cmd>q<cr>", desc = "Quit" }, -- no need to specify mode since it's inherited
		{ "<leader>w", "<cmd>w<cr>", desc = "Write" },
	}
})

vim.cmd([[
  colorscheme space-nvim
  filetype on
  filetype plugin on
  filetype indent on
]])

vim.wo.number = true

require 'nvim-treesitter.configs'.setup {
	-- A list of parser names, or "all" (the listed parsers MUST always be installed)
	ensure_installed = {
		"bash",
		"dockerfile",
		"haskell",
		"haskell_persistent",
		"idris",
		"jq",
		"json",
		"just",
		"lua",
		"luadoc",
		"markdown",
		"markdown_inline",
		"python",
		"query",
		"regex",
		"rust",
		"scala",
		"terraform",
		"tmux",
		"vim",
		"vim",
		"vimdoc",
		"yaml"
	},

	-- Install parsers synchronously (only applied to `ensure_installed`)
	sync_install = false,

	-- Automatically install missing parsers when entering buffer
	-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
	auto_install = true,

	-- List of parsers to ignore installing (or "all")
	ignore_install = { "javascript" },

	---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
	-- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

	highlight = {
		enable = true,

		-- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
		-- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
		-- the name of the parser)
		-- list of language that will be disabled
		disable = { "c" },
		-- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
		disable = function(lang, buf)
			local max_filesize = 100 * 1024 -- 100 KB
			local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
			if ok and stats and stats.size > max_filesize then
				return true
			end
		end,

		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},
}

local neogit = require('neogit')
neogit.setup {}
