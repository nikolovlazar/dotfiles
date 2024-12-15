local wezterm = require("wezterm")

local config = wezterm.config_builder()

-- Font settings
config.font = wezterm.font("DankMono Nerd Font")
config.font_size = 18
config.line_height = 1.2

-- Colors
config.colors = {}

-- Appearance
config.window_decorations = "RESIZE"
config.window_background_image = "/Users/lazarnikolov/dotfiles/.config/kitty/bg-blurred-darker.png"
config.macos_window_background_blur = 30
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

return config
