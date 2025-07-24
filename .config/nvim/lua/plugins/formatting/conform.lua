return {
  {
    'stevearc/conform.nvim',
    opts = function()
      local opts = {
        formatters_by_ft = {
          lua = { 'stylua' },
          php = { 'pint' },
          blade = { 'blade-formatter' },
        },
        formatters = {
          pint = {
            command = 'vendor/bin/pint',
            args = { '$FILENAME' },
            stdin = false,
          },
        },
      }

      opts.format_after_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end

        local filetype = vim.bo[bufnr].filetype
        local prettier_supported = {
          javascript = true,
          javascriptreact = true,
          typescript = true,
          typescriptreact = true,
          json = true,
          jsonc = true,
          css = true,
          graphql = true,
          handlebars = true,
          html = true,
          less = true,
          scss = true,
          vue = true,
          yaml = true,
        }

        if prettier_supported[filetype] then
          return {
            formatters = { 'prettier', 'prettierd' },
            timeout_ms = 2000,
          }
        end

        return { lsp_format = 'fallback' }
      end

      return opts
    end,
  },
}
