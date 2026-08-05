return {
  { -- Treesitter parsers and queries.
    --
    -- On the `main` branch this plugin only installs parsers/queries; the
    -- features themselves come from Neovim, so highlighting and indentation
    -- are opted into per buffer in the FileType autocommand below. Folding is
    -- deliberately left alone -- nvim-ufo owns it (see plugins/editor/ufo).
    'nvim-treesitter/nvim-treesitter',
    branch = 'main',
    lazy = false, -- `main` does not support lazy-loading
    build = ':TSUpdate',
    config = function()
      vim.filetype.add {
        pattern = {
          ['config'] = 'dosini', -- better syntax highlighting for config files
        },
      }

      local ts = require 'nvim-treesitter'

      -- Parsers and queries land in `stdpath('data')/site` by default.
      ts.setup()

      local ensure_installed = {
        'astro',
        'bash',
        'c',
        'css',
        'diff',
        'dockerfile',
        'editorconfig',
        'gitignore',
        'go',
        'gomod',
        'gosum',
        'gowork',
        'html',
        'javascript',
        'json',
        'lua',
        'luadoc',
        'markdown',
        'markdown_inline',
        'python',
        'sql',
        'tsx',
        'typescript',
        'vim',
        'vimdoc',
        'yaml',
      }

      -- Install only what's listed above; no surprise background installs.
      local installed = ts.get_installed 'parsers'
      local missing = vim.tbl_filter(function(lang)
        return not vim.tbl_contains(installed, lang)
      end, ensure_installed)
      if #missing > 0 then
        ts.install(missing)
      end

      local group =
        vim.api.nvim_create_augroup('treesitter-start', { clear = true })

      vim.api.nvim_create_autocmd('FileType', {
        group = group,
        callback = function(ev)
          local lang = vim.treesitter.language.get_lang(ev.match)
          if not lang or not vim.treesitter.language.add(lang) then
            return
          end

          vim.treesitter.start(ev.buf, lang)

          -- Without an indents query `indentexpr()` returns 0 for every line,
          -- so fall back to whatever the ftplugin set up.
          if vim.treesitter.query.get(lang, 'indents') then
            vim.bo[ev.buf].indentexpr =
              "v:lua.require'nvim-treesitter'.indentexpr()"
          end
        end,
      })
    end,
  },
}
-- vim: ts=2 sts=2 sw=2 et
