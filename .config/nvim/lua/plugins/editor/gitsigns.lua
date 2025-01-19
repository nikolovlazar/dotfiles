-- Here is a more advanced example where we pass configuration
-- options to `gitsigns.nvim`. This is equivalent to the following Lua:
--    require('gitsigns').setup({ ... })
--
-- See `:help gitsigns` to understand what the configuration keys do
return {
  { -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = function(_, opts)
      opts.signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      }
      opts.signs_staged = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      }
      opts.on_attach = function(bufnr)
        local gitsigns = require 'gitsigns'

        local function map(mode, l, r, options)
          options = options or {}
          options.buffer = bufnr
          vim.keymap.set(mode, l, r, options)
        end

        -- Navigation
        map('n', ']c', function()
          if vim.wo.diff then
            vim.cmd.normal { ']c', bang = true }
          else
            gitsigns.nav_hunk 'next'
          end
        end, { desc = 'Jump to next git [c]hange' })

        map('n', '[c', function()
          if vim.wo.diff then
            vim.cmd.normal { '[c', bang = true }
          else
            gitsigns.nav_hunk 'prev'
          end
        end, { desc = 'Jump to previous git [c]hange' })

        -- Actions
        -- visual mode
        map('v', '<leader>ghs', function()
          gitsigns.stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'Stage' })
        map('v', '<leader>ghr', function()
          gitsigns.reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'Reset' })
        -- normal mode
        map('n', '<leader>ghs', gitsigns.stage_hunk, { desc = 'Stage' })
        map('n', '<leader>ghr', gitsigns.reset_hunk, { desc = 'Reset' })
        map('n', '<leader>ghu', gitsigns.undo_stage_hunk, { desc = 'Undo' })
        map('n', '<leader>ghp', gitsigns.preview_hunk, { desc = 'Preview' })
        map(
          'n',
          '<leader>gbs',
          gitsigns.stage_buffer,
          { desc = 'Buffer Stage' }
        )
        map(
          'n',
          '<leader>gbr',
          gitsigns.reset_buffer,
          { desc = 'Buffer Reset' }
        )
        map('n', '<leader>gbl', gitsigns.blame_line, { desc = 'Blame Line' })
        map('n', '<leader>gdi', gitsigns.diffthis, { desc = 'Diff Index' })
        map('n', '<leader>gdc', function()
          gitsigns.diffthis '@'
        end, { desc = 'Diff Commit' })
        -- Toggles
        map(
          'n',
          '<leader>gbl',
          gitsigns.toggle_current_line_blame,
          { desc = 'Blame Line' }
        )
        map(
          'n',
          '<leader>gds',
          gitsigns.toggle_deleted,
          { desc = 'Deleted Show' }
        )
      end
    end,
  },
}
-- vim: ts=2 sts=2 sw=2 et
