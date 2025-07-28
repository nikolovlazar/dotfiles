return {
  {
    'nvim-treesitter/nvim-treesitter',
    config = function()
      vim.filetype.add {
        extension = {
          blade = 'blade',
        },
        pattern = {
          ['.*%.blade%.php'] = 'blade',
        },
      }

      vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
        pattern = '*.blade.php',
        callback = function()
          vim.bo.filetype = 'blade'
          -- for some reason, blade highlights wasn't turned on so had to do this
          vim.defer_fn(function()
            if vim.bo.filetype == 'blade' then
              vim.cmd 'TSBufEnable highlight'
            end
          end, 100)
        end,
      })
    end,
  },
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        intelephense = {
          filetypes = { 'php', 'blade', 'php_only' },
          settings = {
            intelephense = {
              files = {
                associations = { '*.php', '*.blade.php' },
                maxSize = 5000000,
              },
            },
          },
        },
      },
    },
  },
  {
    'mfussenegger/nvim-dap',
    optional = true,
    opts = function()
      local dap = require 'dap'
      local path = vim.fn.expand '$MASON/packages/php-debug-adapter'
      dap.adapters.php = {
        type = 'executable',
        command = 'node',
        args = { path .. '/extension/out/phpDebug.js' },
      }
    end,
  },
}
