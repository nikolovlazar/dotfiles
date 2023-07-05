require("nvterm").setup({
	terminals = {
		type_opts = {
			float = {
				row = 0,
				col = 0,
				width = 1,
				height = 0.85,
				border = "rounded",
			},
			vertical = {
				location = "rightbelow",
				split_ratio = 0.4,
			},
		},
	},
})

local toggle_modes = { "n", "t" }
local mappings = {
	{
		toggle_modes,
		"<C-`>",
		function()
			require("nvterm.terminal").toggle("horizontal")
		end,
	},
	{
		toggle_modes,
		"<C-v>",
		function()
			require("nvterm.terminal").toggle("vertical")
		end,
	},
	{
		toggle_modes,
		"<S-i>",
		function()
			require("nvterm.terminal").toggle("float")
		end,
	},
}

local opts = { noremap = true, silent = true }
for _, mapping in ipairs(mappings) do
	vim.keymap.set(mapping[1], mapping[2], mapping[3], opts)
end
