local lsp = require("lsp-zero")
local mason = require("mason-lspconfig")

lsp.preset("recommended")

mason.setup({
	ensure_installed = {
		"lua_ls",
		"cssls",
		"tsserver",
	},
	automatic_installation = true,
})

lsp.nvim_workspace()

lsp.set_preferences({
	suggest_lsp_servers = false,
})

lsp.set_sign_icons({
	error = "✘",
	warn = "▲",
	hint = "⚑",
	info = "»",
})

---@diagnostic disable-next-line: unused-local
lsp.on_attach(function(client, bufnr)
	vim.keymap.set("n", "gd", function()
		vim.lsp.buf.definition()
	end, { buffer = bufnr, remap = false, desc = "Go to definition" })

	vim.keymap.set("n", "K", function()
		vim.lsp.buf.hover()
	end, { buffer = bufnr, remap = false, desc = "Hover" })

	vim.keymap.set("n", "<leader>vws", function()
		vim.lsp.buf.workspace_symbol()
	end, { buffer = bufnr, remap = false, desc = "View workspace symbols" })

	vim.keymap.set("n", "<leader>vd", function()
		vim.diagnostic.open_float()
	end, { buffer = bufnr, remap = false, desc = "View diagnostics" })

	vim.keymap.set("n", "[d", function()
		vim.diagnostic.goto_next()
	end, { buffer = bufnr, remap = false, desc = "View next diagnostic" })

	vim.keymap.set("n", "]d", function()
		vim.diagnostic.goto_prev()
	end, { buffer = bufnr, remap = false, desc = "View previous diagnostic" })

	vim.keymap.set("n", "<leader>ca", function()
		require("cosmic-ui").code_actions()
	end, { buffer = bufnr, remap = false, desc = "Show code actions" })

	vim.keymap.set("v", "<leader>ca", function()
		require("cosmic-ui").range_code_actions()
	end, { buffer = bufnr, remap = false, desc = "Show code actions" })

	vim.keymap.set("n", "<leader>vrr", function()
		vim.lsp.buf.references()
	end, { buffer = bufnr, remap = false, desc = "View references" })

	vim.keymap.set("n", "<leader>vrn", function()
		vim.lsp.buf.rename()
	end, { buffer = bufnr, remap = false, desc = "Rename" })

	vim.keymap.set("i", "<C-h>", function()
		vim.lsp.buf.signature_help()
	end, { buffer = bufnr, remap = false, desc = "Signature help" })
end)

require("lspconfig").tsserver.setup({
	filetypes = {
		"javascript",
		"javascriptreact",
		"javascript.jsx",
		"typescript",
		"typescriptreact",
		"typescript.tsx",
	},
})

lsp.setup()

local ls = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()

vim.keymap.set({ "i", "s" }, "<c-k>", function()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump()
	end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<c-j>", function()
	if ls.jumpable(-1) then
		ls.jump(-1)
	end
end, { silent = true })

local cmp = require("cmp")

local cmp_mappings = lsp.defaults.cmp_mappings({
	-- Disable <Tab> and <S-Tab>, as they conflict with GitHub Copilot
	-- It is still possible to navigate with Arrow Up and Arrown Down
	["<Tab>"] = cmp.config.disable,
	["<S-Tab>"] = cmp.config.disable,
	["<Enter>"] = cmp.mapping.confirm({ select = true }),
	["<C-Space>"] = function()
		if cmp.visible() then
			cmp.close()
		else
			cmp.complete()
		end
	end,
})

cmp.setup({
	mapping = cmp_mappings,
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	sources = cmp.config.sources({
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
	}, {
		{ name = "buffer" },
	}),
	formatting = {
		fields = { "kind", "abbr", "menu" },

		format = function(entry, vim_item)
			if vim.tbl_contains({ "path" }, entry.source.name) then
				local icon, hl_group = require("nvim-web-devicons").get_icon(entry:get_completion_item().label)
				if icon then
					vim_item.kind = icon
					vim_item.kind_hl_group = hl_group
					return vim_item
				end
			end

			local kind = require("lspkind").cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry, vim_item)
			local strings = vim.split(kind.kind, "%s", { trimempty = true })
			kind.kind = " " .. (strings[1] or "") .. " "
			kind.menu = "    (" .. (strings[2] or "") .. ")"

			local twitem = require("tailwindcss-colorizer-cmp").formatter(entry, kind)

			return twitem
		end,
	},
})

vim.diagnostic.config({
	virtual_text = true,
})
