-- LSP configuration, Neovim 0.11 native style.
--
-- Per-server config lives in `~/.config/nvim/lsp/<server>.lua` (read directly
-- by `vim.lsp.config`). nvim-lspconfig is kept ONLY for its bundled default
-- `lsp/` files (cmd / filetypes / root_markers); our files merge on top.
-- `vim.lsp.enable{...}` activates servers. mason + mason-tool-installer just
-- install the binaries. There is no mason-lspconfig bridge anymore.
return {
  {
    -- Lua LSP for editing this config: completion, annotations, signatures.
    'folke/lazydev.nvim',
    ft = 'lua',
    opts = {
      library = {
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      { 'williamboman/mason.nvim', opts = {} },
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      'saghen/blink.cmp',
    },
    config = function()
      -- Servers to activate. Servers without a file in `lsp/` use
      -- nvim-lspconfig's bundled defaults as-is.
      local servers = {
        'lua_ls',
        'eslint',
        'vtsls',
        'tailwindcss',
        'cssls',
        'gopls',
        'pyright',
        'ruff',
        'astro',
        'dockerls',
        'docker_compose_language_service',
        'neocmake',
      }

      -- Broadcast completion capabilities (blink.cmp) + ufo folding to every
      -- server via the wildcard config, so individual `lsp/*.lua` files don't
      -- have to repeat it.
      local capabilities = require('blink.cmp').get_lsp_capabilities()
      capabilities.textDocument.foldingRange =
        { dynamicRegistration = false, lineFoldingOnly = true }
      vim.lsp.config('*', { capabilities = capabilities })

      vim.lsp.enable(servers)

      -- Buffer-local keymaps + behavior when a server attaches. Neovim 0.11
      -- already provides defaults (K hover, grn rename, gra code action,
      -- grr refs, gri impl, gO symbols, C-s signature, [d/]d diagnostics);
      -- we only add definition/type navigation via fzf-lua and a few extras.
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('lsp-attach', { clear = true }),
        callback = function(event)
          local map = function(keys, func, desc, mode)
            vim.keymap.set(
              mode or 'n',
              keys,
              func,
              { buffer = event.buf, desc = 'LSP: ' .. desc }
            )
          end

          map('gd', function()
            require('fzf-lua').lsp_definitions { jump1 = true, ignore_current_line = true }
          end, 'Goto Definition')
          map('gy', function()
            require('fzf-lua').lsp_typedefs { jump1 = true, ignore_current_line = true }
          end, 'Goto Type Definition')
          map('grr', function()
            require('fzf-lua').lsp_references { jump1 = true, ignore_current_line = true }
          end, 'References')
          map('gri', function()
            require('fzf-lua').lsp_implementations { jump1 = true, ignore_current_line = true }
          end, 'Goto Implementation')
          map('<leader>ca', vim.lsp.buf.code_action, 'Code Action', { 'n', 'x' })

          local client = vim.lsp.get_client_by_id(event.data.client_id)

          -- Highlight references of the word under the cursor. Gated on
          -- CursorHold (debounced by `updatetime`), so it is cheap.
          if
            client
            and client:supports_method(
              vim.lsp.protocol.Methods.textDocument_documentHighlight
            )
          then
            local hl = vim.api.nvim_create_augroup(
              'lsp-highlight',
              { clear = false }
            )
            vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
              buffer = event.buf,
              group = hl,
              callback = vim.lsp.buf.document_highlight,
            })
            vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
              buffer = event.buf,
              group = hl,
              callback = vim.lsp.buf.clear_references,
            })
            vim.api.nvim_create_autocmd('LspDetach', {
              group = vim.api.nvim_create_augroup(
                'lsp-detach',
                { clear = true }
              ),
              callback = function(event2)
                vim.lsp.buf.clear_references()
                vim.api.nvim_clear_autocmds {
                  group = 'lsp-highlight',
                  buffer = event2.buf,
                }
              end,
            })
          end

          -- Inlay hints are off by default; this toggles them per-buffer.
          if
            client
            and client:supports_method(
              vim.lsp.protocol.Methods.textDocument_inlayHint
            )
          then
            map('<leader>th', function()
              vim.lsp.inlay_hint.enable(
                not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf }
              )
            end, 'Toggle Inlay Hints')
          end
        end,
      })

      -- Install the binaries the servers + formatters need.
      require('mason-tool-installer').setup {
        ensure_installed = {
          -- language servers
          'lua-language-server',
          'vtsls',
          'eslint-lsp',
          'tailwindcss-language-server',
          'css-lsp',
          -- gopls is provided by mise (go: backend) so it lands on PATH as a
          -- shim; mason can't install it because mise overrides GOBIN.
          'pyright',
          'ruff',
          'astro-language-server',
          'dockerfile-language-server',
          'docker-compose-language-service',
          'neocmakelsp',
          -- formatters
          'stylua',
          'prettier',
          'prettierd',
          'biome',
        },
      }
    end,
  },
}
-- vim: ts=2 sts=2 sw=2 et
