-- Lua language server. `cmd`, `filetypes`, and `root_markers` are inherited
-- from nvim-lspconfig's bundled `lsp/lua_ls.lua`; we only override settings.
return {
  settings = {
    Lua = {
      completion = {
        callSnippet = 'Replace',
      },
      -- Toggle below to silence lua_ls's noisy `missing-fields` warnings:
      -- diagnostics = { disable = { 'missing-fields' } },
    },
  },
}
