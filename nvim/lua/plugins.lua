-- Main plugin specification aggregator
-- This file imports all individual plugin specifications from the plugins/ directory

-- Import all plugins by requiring each module
-- The plugins directory contains individual plugin specifications following lazy.nvim format

local plugins = {}

-- Import all plugin files
-- Core and UI plugins
plugins[#plugins + 1] = require("plugins.align")
plugins[#plugins + 1] = require("plugins.appearance")
plugins[#plugins + 1] = require("plugins.autopairs")
plugins[#plugins + 1] = require("plugins.claude")
plugins[#plugins + 1] = require("plugins.cmp")
plugins[#plugins + 1] = require("plugins.codecomplete")
plugins[#plugins + 1] = require("plugins.code-feedback")
plugins[#plugins + 1] = require("plugins.comment")
plugins[#plugins + 1] = require("plugins.conform")
plugins[#plugins + 1] = require("plugins.copilot")
plugins[#plugins + 1] = require("plugins.dap")
plugins[#plugins + 1] = require("plugins.diagnostics")
plugins[#plugins + 1] = require("plugins.fff")
plugins[#plugins + 1] = require("plugins.flash")
plugins[#plugins + 1] = require("plugins.gitsigns")
plugins[#plugins + 1] = require("plugins.indent-object")
plugins[#plugins + 1] = require("plugins.jj-diffconflicts")
plugins[#plugins + 1] = require("plugins.jujube")
plugins[#plugins + 1] = require("plugins.lsp")
plugins[#plugins + 1] = require("plugins.lualine")
plugins[#plugins + 1] = require("plugins.marks")
plugins[#plugins + 1] = require("plugins.matchup")
plugins[#plugins + 1] = require("plugins.neogit")
plugins[#plugins + 1] = require("plugins.performance")
plugins[#plugins + 1] = require("plugins.precognition")
plugins[#plugins + 1] = require("plugins.quickscope")
plugins[#plugins + 1] = require("plugins.spectre")
plugins[#plugins + 1] = require("plugins.startup")
plugins[#plugins + 1] = require("plugins.startuptime")
plugins[#plugins + 1] = require("plugins.surround")
plugins[#plugins + 1] = require("plugins.targets")
plugins[#plugins + 1] = require("plugins.telescope")
plugins[#plugins + 1] = require("plugins.text-case")
plugins[#plugins + 1] = require("plugins.theme")
plugins[#plugins + 1] = require("plugins.trailing-space")
plugins[#plugins + 1] = require("plugins.trailspace")
plugins[#plugins + 1] = require("plugins.treesitter")
plugins[#plugins + 1] = require("plugins.ufo")
plugins[#plugins + 1] = require("plugins.visual-star")
plugins[#plugins + 1] = require("plugins.which-key")

return plugins
