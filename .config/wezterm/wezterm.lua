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
local function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'custom-mocha'
  else
    return 'custom-latte'
  end
end

local custom_mocha = wezterm.color.get_builtin_schemes()['Catppuccin Mocha']
custom_mocha.background = '#000000'

local custom_latte = wezterm.color.get_builtin_schemes()['Catppuccin Latte']
custom_latte.background = '#FFFFFF'

config.color_schemes = {
  ['custom-mocha'] = custom_mocha,
  ['custom-latte'] = custom_latte,
}
config.color_scheme = scheme_for_appearance(wezterm.gui.get_appearance())

-- Invoke toggle-theme command when config reloads, which happens when the system color mode changes
local toggle_theme_command = require 'commands.toggle-theme'
wezterm.on('window-config-reloaded', function(window, pane)
  window:perform_action(toggle_theme_command.action, pane)
end)

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
