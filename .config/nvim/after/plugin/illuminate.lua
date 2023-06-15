require("illuminate").configure({
	providers = { "lsp", "treesitter" },
})

vim.keymap.set("n", "<S-n>", require("illuminate").goto_next_reference, { desc = "Move to next reference" })
vim.keymap.set("n", "<S-p>", require("illuminate").goto_prev_reference, { desc = "Move to previous reference" })
vim.keymap.set("o", "<S-i>", require("illuminate").textobj_select)
vim.keymap.set("x", "<S-i>", require("illuminate").textobj_select)
