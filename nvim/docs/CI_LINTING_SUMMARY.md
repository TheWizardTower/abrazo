# CI Lua Linting Implementation Summary

## Changes Made

### 1. Updated CI Workflow (`.github/workflows/lint.yml`)

**Job**: `neovim`
**Name**: `neovim (lua parse-check + stylua + luacheck)`

**New Step Added:**
```yaml
- name: luacheck (lint)
  run: |
    cd nvim
    luacheck . --exclude lazy-lock --max-line-length 120 || { echo "luacheck failed"; exit 1; }
```

**Full Job Now Includes:**
1. Install lua5.1, stylua, and luacheck (via luarocks)
2. Parse check with `luac5.1 -p`
3. Style check with `stylua --check`
4. **Linting with `luacheck`** ⭐ NEW

### 2. Created Luacheck Configuration (`nvim/.luacheckrc`)

- Allows expected globals: `vim`, `unpack`
- Sets max line length to 120 (Neovim style)
- Excludes third-party dependencies
- Configured for Neovim-specific patterns

### 3. Created Local Lint Script (`nvim/scripts/lint-lua.sh`)

```bash
#!/bin/bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)/nvim"
luacheck . \
    --config .luacheckrc \
    --exclude lazy-lock \
    --max-line-length 120
```

### 4. Added Documentation (`docs/LINTING.md`)

Comprehensive guide for:
- What's checked and why
- Running locally (script + manual commands)
- CI integration details
- Adding new Lua files

## Files Created/Modified

| File | Type |
|------|------|
| `.github/workflows/lint.yml` | Modified - Added luacheck step |
| `nvim/.luacheckrc` | New - Luacheck configuration |
| `nvim/scripts/lint-lua.sh` | New - Local lint script |
| `docs/LINTING.md` | New - Linting documentation |

## CI Run Triggers

The lint checks run automatically on:
- Every push to master branch
- All pull requests  
- Weekly (Mondays 04:00 UTC) for drift detection

## Benefits

✅ **Early bug detection** - Catches syntax errors before merge  
✅ **Consistent formatting** - Enforces StyLua style guide  
✅ **Code quality** - Luacheck identifies anti-patterns  
✅ **Self-documenting** - Clear lint rules in `.luacheckrc`  

## Local Development Flow

```bash
# Before committing:
./nvim/scripts/lint-lua.sh

# Or manually:
stylua --check nvim/
luac5.1 -p nvim/**/*.lua 2>&1 | grep -v "lazy-lock"
```

## Troubleshooting CI Failures

If luacheck fails in CI:

1. Check the error message for file/line
2. Run locally with same command:
   ```bash
   cd nvim && luacheck .
   ```
3. Fix the issue (unused variable, undefined global, etc.)
4. Consider updating `.luacheckrc` if it's a false positive

