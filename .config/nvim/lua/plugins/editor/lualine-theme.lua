local M = {}

local function get_palette()
  local palette = require('catppuccin.palettes').get_palette 'mocha'

  return {
    bg = palette.base,
    fg = palette.text,
    yellow = palette.yellow,
    cyan = palette.teal,
    darkblue = palette.crust,
    green = palette.green,
    orange = palette.peach,
    violet = palette.lavender,
    magenta = palette.mauve,
    blue = palette.blue,
    red = palette.red,
    pink = palette.pink,
  }
end

function M.build_theme()
  return get_palette()
end

function M.apply_highlights()
  local c = get_palette()

  vim.api.nvim_set_hl(0, 'LualineNormalC', { fg = c.fg, bg = c.bg })
  vim.api.nvim_set_hl(0, 'LualineInactiveC', { fg = c.fg, bg = c.bg })
  vim.api.nvim_set_hl(0, 'LualineFilename', { fg = c.fg, bg = c.bg })

  vim.api.nvim_set_hl(0, 'LualineDiagnosticsError', { bg = c.bg, fg = c.red })
  vim.api.nvim_set_hl(0, 'LualineDiagnosticsWarn', { bg = c.bg, fg = c.yellow })
  vim.api.nvim_set_hl(0, 'LualineDiagnosticsInfo', { bg = c.bg, fg = c.cyan })
  vim.api.nvim_set_hl(0, 'LualineLsp', { bg = c.bg, fg = c.pink })
  vim.api.nvim_set_hl(
    0,
    'LualineBranch',
    { bg = c.bg, fg = c.violet, bold = true }
  )
  vim.api.nvim_set_hl(
    0,
    'LualineDiffAdded',
    { bg = c.bg, fg = c.green, bold = true }
  )
  vim.api.nvim_set_hl(
    0,
    'LualineDiffModified',
    { bg = c.bg, fg = c.orange, bold = true }
  )
  vim.api.nvim_set_hl(
    0,
    'LualineDiffRemoved',
    { bg = c.bg, fg = c.red, bold = true }
  )

  -- add more as needed
end

return M
