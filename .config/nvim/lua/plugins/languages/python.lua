local lsp = 'pyright'
local ruff = 'ruff'

return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = { ensure_installed = { 'ninja', 'rst' } },
  },
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        ruff = {
          cmd_env = { RUFF_TRACE = 'messages' },
          init_options = {
            settings = {
              logLevel = 'error',
            },
          },
          keys = {
            {
              '<leader>co',
              vim.lsp.buf.code_action {
                apply = true,
                context = {
                  only = { 'source.organizeImports' },
                  diagnostics = {},
                },
              },
              desc = 'Organize Imports',
            },
          },
        },
        ruff_lsp = {
          keys = {
            {
              '<leader>co',
              vim.lsp.buf.code_action {
                apply = true,
                context = {
                  only = { 'source.organizeImports' },
                  diagnostics = {},
                },
              },
              desc = 'Organize Imports',
            },
          },
        },
      },
      setup = {
        [ruff] = function()
          local on_attach = function(client, _)
            -- Disable hover in favor of Pyright
            client.server_capabilities.hoverProvider = false
          end
          local name = ruff
          vim.api.nvim_create_autocmd('LspAttach', {
            callback = function(args)
              local buffer = args.buf ---@type number
              local client = vim.lsp.get_client_by_id(args.data.client_id)
              if client and (not name or client.name == name) then
                return on_attach(client, buffer)
              end
            end,
          })
        end,
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    opts = function(_, opts)
      local servers =
        { 'pyright', 'basedpyright', 'ruff', 'ruff_lsp', ruff, lsp }
      for _, server in ipairs(servers) do
        opts.servers[server] = opts.servers[server] or {}
        opts.servers[server].enabled = server == lsp or server == ruff
      end
    end,
  },
  {
    'nvim-neotest/neotest',
    optional = true,
    dependencies = {
      'nvim-neotest/neotest-python',
    },
    opts = {
      adapters = {
        ['neotest-python'] = {
          -- Here you can specify the settings for the adapter, i.e.
          -- runner = "pytest",
          -- python = ".venv/bin/python",
        },
      },
    },
  },
  {
    'hrsh7th/nvim-cmp',
    optional = true,
    opts = function(_, opts)
      opts.auto_brackets = opts.auto_brackets or {}
      table.insert(opts.auto_brackets, 'python')
    end,
  },
  {
    'mfussenegger/nvim-dap',
    optional = true,
    dependencies = {
      'mfussenegger/nvim-dap-python',
      -- stylua: ignore
      keys = {
        { "<leader>dPt", function() require('dap-python').test_method() end, desc = "Debug Method", ft = "python" },
        { "<leader>dPc", function() require('dap-python').test_class() end, desc = "Debug Class", ft = "python" },
      },
      config = function()
        if vim.fn.has 'win32' == 1 then
          require('dap-python').setup(
            vim.env.MASON
              .. '/packages/'
              .. 'debugpy'
              .. '/venv/Scripts/pythonw.exe'
          )
        else
          require('dap-python').setup(
            vim.env.MASON .. '/packages/' .. 'debugpy' .. '/venv/bin/python'
          )
        end
      end,
    },
  },
  -- Don't mess up DAP adapters provided by nvim-dap-python
  {
    'jay-babu/mason-nvim-dap.nvim',
    optional = true,
    opts = {
      handlers = {
        python = function() end,
      },
    },
  },
  {
    'linux-cultist/venv-selector.nvim',
    branch = 'regexp',
    dependencies = {
      'neovim/nvim-lspconfig',
      'nvim-telescope/telescope.nvim',
      'mfussenegger/nvim-dap-python',
    },
    opts = {},
    keys = {
      -- Keymap to open VenvSelector to pick a venv.
      { '<leader>vs', '<cmd>VenvSelect<cr>' },
      -- Keymap to retrieve the venv from a cache (the one previously used for the same project directory).
      { '<leader>vc', '<cmd>VenvSelectCached<cr>' },
    },
  },
}
