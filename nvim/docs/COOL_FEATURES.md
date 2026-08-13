# Neovim Config - Cool Features You're Already Using (or Could Be!)

## 🎯 What's Already Set Up

### 1. **Copilot AI Integration** 
- `copilot.lua` + `copilot-cmp` for smart completions
- `CopilotChat.nvim` for conversational coding (`:CopilotChat`)
- Custom provider pointing to your local LLM server at turkishDelight:8080

### 2. **CodeCompanion** 
- `/lua/plugins/codecompanion.lua`
- AI pair programmer that can answer questions about your code
- Also uses your local turkishDelight server
- Usage: `:CodeCompanion chat "How do I..."`

### 3. **LSP Diagnostics with Navigation**
- Custom icons for errors/warnings/hints/info
- `<leader>ld` - Open diagnostics in floating window
- `[d`, `]d` - Navigate between diagnostics
- `<leader>lq` - Quickfix list of all diagnostics

### 4. **Git Workflow (Gitsigns + Neogit)**
**Gitsigns:**
- Visual indicators for changed lines in git repo
- `<leader>hs` / `]c` - Stage hunk, go to next hunk
- `<leader>hb` - Blame line with full details

**Neogit:**
- Full Git UI inside Neovim (`<leader>pg`)
- Uses diffview.nvim for beautiful diffs

### 5. **Code Folding (UFO + Treesitter)**
- `ufo.lua` sets up smart folding
- `<leader>uf` - Toggle UFO folding
- Shows code structure via treesitter and indent levels

### 6. **AI Chat in Insert Mode**
**Copilot Suggestion:**
- Type something, Copilot suggests the rest (ghost text)
- `<C-j>` to accept next suggestion
- See `/lua/plugins/copilot.lua`

## 🚀 Features You Could Use More

### 7. **Spectre Search/Replace**
A visual search and replace tool that's much better than basic `:%s///`:
```vim
<leader>sr  # Open Spectre search/replace
<leader>sw  # Search word under cursor
<leader>sf  # Search in current file
```

### 8. **Marks.nvim**
Track your location history with marks:
- `<Tab>` - Jump to next mark (default binding)
- Shows marks visually in the buffer

### 9. **Flash.nvim**
"Jump to anywhere on screen" motions:
- `s` + letter - Jump to visible text
- `S` - Backwards jump
- Works with treesitter selections (`<leader>v`)
```

### 10. **Align (mini.align)**
Quick alignment of code:
```vim
gl        # Start align mode
gL        # Align with preview first
```
Example: Select multiple lines and align equals signs, commas, etc.

### 11. **Text Case.nvim**
Change text case with Telescope integration:
- `<leader>tc` - Change text case (uses Telescope picker)
- Supports camelCase, snake_case, kebab-case, etc.

### 12. **Comment.nvim + ts-context-commentstring**
Smart commenting that respects language context:
```vim
gcc       # Toggle comment current line
gbc       # Block comment selection
gc{motion} # Comment with given motion (e.g., gcap = paragraph)
```

### 13. **Visual Star Search (vim-visual-star-search)**
Search for selected text in visual mode:
- Select word, press `*` to search forward
- Press `#` to search backward

### 14. **Targets.vim + nvim-surround**
Enhanced text objects:
```vim
ci"       # Change inside quotes (you have surround)
yip       # Yank inner paragraph (targets.vim)
dab       # Delete around block (surround)
```

### 15. **Quickscope**
Highlight "next/prev" positions for f/F/t/T motions:
- Press `f` or `t`, see colored hints
- Jump to any position in the line with just a few keystrokes

## 📋 Quick Reference: Key Bindings

| Mapping | Purpose |
|---------|---------|
| `<leader>sr` | Spectre search/replace (visual) |
| `<leader>pg` | Neogit status window |
| `<leader>hs` / `]c` | Gitsigns stage hunk, next hunk |
| `<leader>ld` | Open diagnostics float |
| `[d`, `]d` | Navigate diagnostics |
| `<F5>`-`<F12>` | DAP debug controls |
| `gcc`, `gbc` | Comment toggle/block |

## 💡 Recommended Next Steps

1. **Try Spectre** - It's amazing for search/replace: `<leader>sr`
2. **Use Flash** - Jump anywhere on screen with `s` + letter
3. **Check marks.nvim** - See where you've been in the file
4. **Enable Quickscope hints** - Make f/F/t/T motions easier

