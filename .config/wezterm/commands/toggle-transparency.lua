local wezterm = require("wezterm")

local command = {
	brief = "Toggle terminal transparency",
	icon = "md_circle_opacity",
	action = wezterm.action_callback(function(window)
		local overrides = window:get_config_overrides() or {}

		if not overrides.window_background_opacity or overrides.window_background_opacity == 1 then
			overrides.window_background_opacity = 0.8
		else
			overrides.window_background_opacity = 1
		end

		window:set_config_overrides(overrides)
	end),
}

return command
