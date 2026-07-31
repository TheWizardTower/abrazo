# Neovim Configuration Capabilities

## AI & Coding Assistance

| Feature | Plugin | Usage |
|---------|--------|-------|
| Copilot completions | copilot.lua + copilot-cmp | Type and see suggestions |
| Copilot Chat | CopilotChat.nvim | `:CopilotChat "question"` |
| CodeCompanion | codecompanion.nvim | `:CodeCompanion chat` |

## Diagnostics & LSP

| Feature | Key Mapping |
|---------|-------------|
| Next diagnostic | `]d` |
| Previous diagnostic | `[d` |
| Open diagnostics float | `<leader>ld` |
| Quickfix list of diagnostics | `<leader>lq` |
| Code actions on selection | `<leader>a` |

## Git Integration

### Gitsigns
- Visual indicators for git changes
- `]c`, `[c` - Next/previous hunk
- `<leader>hs` - Stage hunk, `<leader>hr` - Reset hunk
- `<leader>hp` - Preview hunk diff
- `<leader>hb` - Show blame for current line

### Neogit
- Full Git UI inside Neovim: `:Neogit`
- Uses diffview.nvim for beautiful diffs

## Debugging (DAP)

| Key | Action |
|-----|--------|
| `<F5>` | Continue |
| `<F10>` | Step Over |
| `<F11>` | Step Into |
| `<F12>` | Step Out |
| `<leader>db` | Toggle breakpoint |
| `<leader>dB` | Conditional breakpoint |
| `<leader>dl` | Run last session |

## File Navigation & Search

### Telescope
- `:Telescope find_files` - Find files
- `:Telescope live_grep` - Live grep
- `:Telescope buffers` - Buffers

### FFF (Alternative Finder)
- `<leader>Ff`, `<leader>Fg` - FFF find files/grep

### Spectre (Search/Replace)
- `<leader>sr` - Visual search and replace
- `<leader>sw`, `<leader>sf` - Word/file search

## Code Quality Tools

| Feature | Plugin |
|---------|--------|
| Auto-formatting | conform.nvim |
| Trailing whitespace cleanup | mini.trailspace (auto on save) |

## Text Editing Enhancements

### Commenting
- `gcc` - Toggle line comment
- `gbc` - Block comment selection
- Uses Treesitter context for proper comments

### Surrounding Text
- `ys`, `ds`, `cs` - Add/remove/change surroundings (quotes, parens, etc.)

### Targets & Motions
- Enhanced text objects via targets.vim + nvim-surround
- e.g., `yip` = yank inner paragraph

## Visual Aids

| Feature | Key |
|---------|-----|
| Flash jumps (`s`) | Jump to visible text with letters |
| Precognition hints (disabled by default) | `<leader>tp` to toggle |

### Quickscope
- Shows hints for f/F/t/T motions on screen

### Matchup
- Better paren matching with `%` motion

## UI & Display

| Feature | Plugin |
|---------|--------|
| Statusline | lualine.nvim |
| Code folding | ufo.nvim + treesitter |
| Twilight dimming (context focus) | twilight.nvim |

## Installation Management

- Mason - Install LSP/dap/formatters
- Mason-nvim-dap - DAP adapters
- Lazy.nvim - Plugin management with lazy loading

