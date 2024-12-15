local wezterm = require("wezterm")
local commands = require("commands")

local config = wezterm.config_builder()

-- Font settings
config.font = wezterm.font("DankMono Nerd Font")
config.font_size = 18
config.line_height = 1.2

-- Colors
config.colors = {
	cursor_bg = "white",
}

-- Appearance
config.window_decorations = "RESIZE"
config.window_background_image = wezterm.home_dir .. "/dotfiles/.config/wezterm/assets/bg-blurred-darker.png"
config.macos_window_background_blur = 30
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

-- Miscellaneous settings
config.max_fps = 120
config.prefer_egl = true

wezterm.on("augment-command-palette", function()
	return commands
end)

return config
