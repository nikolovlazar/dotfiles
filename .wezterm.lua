local wezterm = require("wezterm")

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
config.window_background_image = os.getenv("HOME") .. "/dotfiles/.config/kitty/bg-blurred-darker.png"
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

-- Custom commands
wezterm.on("augment-command-palette", function(window, pane)
	return {
		{
			brief = "Toggle terminal transparency",
			icon = "md_circle_opacity",
			action = wezterm.action_callback(function(window)
				local overrides = window:get_config_overrides() or {}

				if not overrides.window_background_opacity or overrides.window_background_opacity == 1 then
					overrides.window_background_opacity = 0.9
				else
					overrides.window_background_opacity = 1
				end

				window:set_config_overrides(overrides)
			end),
		},
	}
end)

return config
