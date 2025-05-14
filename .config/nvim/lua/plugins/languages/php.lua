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
      local path = require('mason-registry')
        .get_package('php-debug-adapter')
        :get_install_path()
      dap.adapters.php = {
        type = 'executable',
        command = 'node',
        args = { path .. '/extension/out/phpDebug.js' },
      }

      if not dap.configurations['php'] then
        dap.configurations['php'] = {
          {
            type = 'php',
            request = 'launch',
            name = 'Listen for XDebug',
            stopOnEntry = true,
            port = 9003,
            pathMappings = (function()
              local cwd = vim.fn.getcwd()
              local real_cwd = vim.loop.fs_realpath(cwd)
              return {
                [real_cwd] = real_cwd,
              }
            end)(),
          },
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
