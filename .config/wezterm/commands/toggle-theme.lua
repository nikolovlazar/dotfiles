local wezterm = require 'wezterm'

local command = {
  brief = 'Toggle theme',
  icon = 'md_theme_light_dark',
  action = wezterm.action_callback(function(window)
    local home = os.getenv 'HOME'

    if window:effective_config().color_scheme == 'custom-latte' then
      wezterm.run_child_process {
        '/bin/bash',
        '-c',
        home .. '/.config/tmux/switch-catppuccin-theme.sh latte',
      }
    else
      wezterm.run_child_process {
        '/bin/bash',
        '-c',
        home .. '/.config/tmux/switch-catppuccin-theme.sh mocha',
      }
    end
  end),
}

return command
