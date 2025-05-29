vim.g.lazyvim_php_lsp = 'intelephense'

local lsp = vim.g.lazyvim_php_lsp or 'intelephense'

return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = {
      ensure_installed = {
        'php',
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        intelephense = {
          enabled = lsp == 'intelephense',
          filetypes = { 'php', 'blade', 'php_only' },
          settings = {
            intelephense = {
              filetypes = { 'php', 'blade', 'php_only' },
              files = {
                associations = { '*.php', '*.blade.php' },
                maxSize = 5000000,
              },
            },
          },
        },
        [lsp] = {
          enabled = true,
        },
      },
    },
  },
  {
    'williamboman/mason.nvim',
    opts = {
      ensure_installed = {
        'blade-formatter',
        'php-cs-fixer',
        'php-debug-adapter',
        'phpcs',
        'phpstan',
        'pint',
      },
    },
  },
  {
    'mfussenegger/nvim-dap',
    optional = true,
    opts = function()
      local dap = require 'dap'
      local path = vim.fn.expand '$MASON/packages/php-debug-adapter/'

      if php_debug_adapter_package == nil then
        vim.notify(
          'Error: php-debug-adapter package not found in Mason registry',
          vim.log.levels.ERROR
        )
      else
        local path = vim.fn.expand '$MASON/packages/php-debug-adapter'
        dap.adapters.php = {
          type = 'executable',
          command = 'node',
          args = { path .. '/extension/out/phpDebug.js' },
        }
      end
    end,
  },
  {
    'mfussenegger/nvim-lint',
    optional = true,
    opts = {
      linters_by_ft = {
        php = { 'phpcs' },
      },
    },
  },
  {
    'stevearc/conform.nvim',
    optional = true,
    opts = {
      formatters_by_ft = {
        php = { 'pint', 'php_cs_fixer' },
      },
    },
  },
}
