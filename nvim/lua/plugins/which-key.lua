return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		config = function()
			require('config.which-key')
		end,
	},

	{
		"nvim-neorg/neorg",
		-- lazy-load on filetype
		ft = "norg",
		-- options for neorg. This will automatically call `require("neorg").setup(opts)`
		opts = {
			load = {
				["core.defaults"] = {},
			},
		},
	},
}
