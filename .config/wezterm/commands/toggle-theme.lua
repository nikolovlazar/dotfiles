local wezterm = require 'wezterm'

local command = {
  brief = 'Toggle theme',
  icon = 'md_theme_light_dark',
  action = wezterm.action_callback(function(window)
    local overrides = window:get_config_overrides() or {}

    if overrides.color_scheme == 'cyberdream-light' then
      overrides.color_scheme = 'cyberdream'
    else
      overrides.color_scheme = 'cyberdream-light'
    end

    window:set_config_overrides(overrides)

    -- Send :DarkLightSwitch to Neovim via known socket
    local shell_cmd = [[
    if [ -S /tmp/nvim-nikolovlazar.sock ]; then
      /opt/homebrew/bin/nvim --server /tmp/nvim-nikolovlazar.sock --remote-send ":DarkLightSwitch<CR>"
    else
      echo "No Neovim socket found" >> /tmp/wezterm_nvim_debug.log
    fi
  ]]

    wezterm.run_child_process { '/bin/bash', '-c', shell_cmd }
  end),
}

return command
