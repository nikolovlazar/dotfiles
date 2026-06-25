-- ESLint LSP. Used for linting/diagnostics; formatting is delegated to conform.
return {
  settings = {
    workingDirectory = { mode = 'auto' }, -- auto-detect project root
  },
  on_attach = function(client, bufnr)
    -- Let conform own formatting, not ESLint.
    client.server_capabilities.documentFormattingProvider = false

    vim.api.nvim_buf_create_user_command(bufnr, 'EslintFixAll', function()
      client:exec_cmd {
        command = 'eslint.applyAllFixes',
        arguments = { { uri = vim.uri_from_bufnr(bufnr) } },
      }
    end, { desc = 'ESLint: Fix all autofixable problems' })
  end,
}
