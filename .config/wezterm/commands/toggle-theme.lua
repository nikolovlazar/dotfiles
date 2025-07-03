local wezterm = require 'wezterm'

local command = {
  brief = 'Toggle theme',
  icon = 'md_theme_light_dark',
  action = wezterm.action_callback(function(window)
    local overrides = window:get_config_overrides() or {}
    local home = os.getenv 'HOME'

    if overrides.color_scheme == 'light' then
      overrides.color_scheme = 'dark'
      wezterm.run_child_process {
        '/bin/bash',
        '-c',
        home .. '/.config/tmux/switch-catppuccin-theme.sh mocha',
      }
    else
      overrides.color_scheme = 'light'
      wezterm.run_child_process {
        '/bin/bash',
        '-c',
        home .. '/.config/tmux/switch-catppuccin-theme.sh latte',
      }
    end

    wezterm.run_child_process {
      '/bin/bash',
      '-c',
      'env > /tmp/wezterm-env.txt; echo hi > /tmp/wezterm-hit.txt',
    }

    window:set_config_overrides(overrides)

    -- Send :DarkLightSwitch to Neovim via known socket
    local nvim_theme_switch = [[
  for socket in /tmp/nvim-nikolovlazar-*.sock; do
    if [ -S "$socket" ]; then
      /opt/homebrew/bin/nvim --server "$socket" --remote-send ":DarkLightSwitch<CR>" 2>/dev/null && \
        echo "✓ $socket" >> /tmp/wezterm_nvim_debug.log || \
        echo "✗ $socket" >> /tmp/wezterm_nvim_debug.log
    fi
  done
]]

    wezterm.run_child_process { '/bin/bash', '-c', nvim_theme_switch }
  end),
}

return command
