local wezterm = require 'wezterm'
local commands = require 'commands'

local config = wezterm.config_builder()

-- Font settings
config.font_size = 16
config.line_height = 1.2
config.font = wezterm.font_with_fallback {
  {
    family = 'Dank Mono',
    harfbuzz_features = {
      'calt',
      'ss01',
      'ss02',
      'ss03',
      'ss04',
      'ss05',
      'ss06',
      'ss07',
      'ss08',
      'ss09',
      'liga',
    },
  },
  { family = 'Symbols Nerd Font Mono' },
}
config.font_rules = {
  {
    italic = true,
    font = wezterm.font('Dank Mono', {
      italic = true,
    }),
  },
}

-- Colors
config.color_schemes = {
  ['cyberdream'] = require 'cyberdream',
  ['cyberdream-light'] = require 'cyberdream-light',
}
config.color_scheme = 'cyberdream'

-- Appearance
config.cursor_blink_rate = 0
config.window_decorations = 'RESIZE'
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.macos_window_background_blur = 40

-- Miscellaneous settings
config.max_fps = 120
config.prefer_egl = true

-- Custom commands
wezterm.on('augment-command-palette', function()
  return commands
end)

return config
