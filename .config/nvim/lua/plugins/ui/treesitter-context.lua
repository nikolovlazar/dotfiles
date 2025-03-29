-- Show context of the current function
return {
  'nvim-treesitter/nvim-treesitter-context',
  event = { 'BufReadPost', 'BufWritePost', 'BufNewFile' },
  opts = function()
    local tsc = require 'treesitter-context'

    local function toggle_treesitter_context()
      if tsc.enabled then
        tsc.disable()
      else
        tsc.enable()
      end
    end

    vim.api.nvim_set_keymap(
      'n',
      '<leader>ut',
      ':lua toggle_treesitter_context()<CR>',
      { noremap = true, silent = true }
    )

    return { mode = 'cursor', max_lines = 3 }
  end,
}
