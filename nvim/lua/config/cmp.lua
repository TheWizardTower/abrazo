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

  -- Window appearance with better positioning
  window = {
    completion = {
      border = 'rounded',
      winhighlight = 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None',
      scrollbar = true,
      -- Position below cursor with offset
      col_offset = 0,
      side_padding = 1,
    },
    documentation = {
      border = 'rounded',
      winhighlight = 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None',
      max_height = 15,
      max_width = 80,
      scrollbar = true,
    },
  },

  -- Completion sources (in priority order)
  sources = cmp.config.sources({
    { name = "copilot",  priority = 1100 },     -- Copilot (highest priority)
    { name = "nvim_lsp", priority = 1000 },     -- LSP completions
    { name = "luasnip",  priority = 750 },      -- Snippets
    { name = "nvim_lua", priority = 650 },      -- Neovim Lua API
  }, {
    { name = "buffer", priority = 500 },        -- Buffer words
    { name = "path",   priority = 250 },        -- File paths
    { name = "calc",   priority = 100 },        -- Calculator
    { name = "spell",  priority = 50 },         -- Spell checking
  }),

  -- Key mappings with Copilot priority
  mapping = cmp.mapping.preset.insert({
    -- Navigate completion menu
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),

    -- Accept completion - only when explicitly selected
    ["<CR>"] = cmp.mapping.confirm({
      select = false,       -- Don't auto-select first item
      behavior = cmp.ConfirmBehavior.Replace,
    }),

    -- Tab: ALWAYS check Copilot first, even when cmp menu is visible
    ["<Tab>"] = cmp.mapping(function(fallback)
      -- Priority 1: Check if Copilot has a suggestion (even if cmp is visible)
      local copilot_ok, copilot_suggestion = pcall(require, "copilot.suggestion")
      if copilot_ok and copilot_suggestion.is_visible() then
        copilot_suggestion.accept()
        -- Priority 2: Navigate cmp menu if it's visible
      elseif cmp.visible() then
        cmp.select_next_item()
        -- Priority 3: Expand or jump to next snippet placeholder
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
        -- Priority 4: Trigger completion if there are words before cursor
      elseif has_words_before() then
        cmp.complete()
        -- Priority 5: Fall back to default Tab behavior
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

    -- Alternative: Use Ctrl+Y to explicitly accept cmp completion
    ["<C-y>"] = cmp.mapping.confirm({
      select = true,
      behavior = cmp.ConfirmBehavior.Replace,
    }),

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

  -- Formatting appearance with Copilot icon
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = lspkind.cmp_format({
      mode = "symbol_text",
      maxwidth = 50,
      ellipsis_char = "...",
      symbol_map = { Copilot = "" },
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

  -- Experimental features - disable ghost text to avoid conflict
  experimental = {
    ghost_text = false,     -- Let Copilot handle ghost text
  },
})

-- Custom autocmd to better position documentation window
vim.api.nvim_create_autocmd("CompleteChanged", {
  pattern = "*",
  callback = function()
    local completed_item = vim.v.completed_item
    if completed_item and completed_item.info then
      -- Documentation will appear, ensure it doesn't cover code
      vim.cmd('stopinsert')
      vim.cmd('startinsert')
    end
  end,
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

-- Add pumheight to limit completion menu height and keep code visible
vim.opt.pumheight = 15 -- Limit completion menu to 15 items max
