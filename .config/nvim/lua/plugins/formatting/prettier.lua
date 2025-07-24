local supported = {
  'css',
  'graphql',
  'handlebars',
  'html',
  'javascript',
  'javascriptreact',
  'json',
  'jsonc',
  'less',
  'scss',
  'typescript',
  'typescriptreact',
  'vue',
  'yaml',
}

return {
  {
    'williamboman/mason.nvim',
    opts = { ensure_installed = { 'prettier' } },
  },
  {
    'stevearc/conform.nvim',
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      for _, ft in ipairs(supported) do
        opts.formatters_by_ft[ft] = opts.formatters_by_ft[ft] or {}
        table.insert(opts.formatters_by_ft[ft], 'prettier')
        table.insert(opts.formatters_by_ft[ft], 'prettierd') -- fallback
      end

      opts.formatters = opts.formatters or {}
      opts.formatters.prettier = opts.formatters.prettier or {}
      opts.formatters.prettierd = opts.formatters.prettierd or {}
    end,
  },
  {
    'nvimtools/none-ls.nvim',
    optional = true,
    opts = function(_, opts)
      local nls = require 'null-ls'
      opts.sources = opts.sources or {}
      table.insert(opts.sources, nls.builtins.formatting.prettier)
    end,
  },
}
