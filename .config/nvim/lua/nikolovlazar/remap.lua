vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = "Back to explorer" })

-- Moving lines with S-j and S-k when in visual mode
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Keep cursor in middle when jumping half pages with C-d and C-u
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Keep cursor in middle when searching
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Format document
vim.keymap.set("n", "<leader>fd", function()
	vim.lsp.buf.format({ async = true })
end, { desc = "Format document" })

-- yank to system clipboard. you can paste anywhere outside vim
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to system clipboard" })
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Something about yanking to system clipboard too but don't use it" })

-- Tabs
vim.keymap.set("n", "<leader>ta", ":tabnew<cr>")
vim.keymap.set("n", "<leader>tx", ":tabclose<cr>")
vim.keymap.set("n", "<leader>tl", ":tabn<cr>")
vim.keymap.set("n", "<leader>th", ":tabp<cr>")
vim.keymap.set("n", "<leader>tmn", ":+tabmove<cr>")
vim.keymap.set("n", "<leader>tmp", ":-tabmove<cr>")

vim.keymap.set(
	"n",
	"<leader>s",
	[[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
	{ desc = "Replace word under cursor" }
)
