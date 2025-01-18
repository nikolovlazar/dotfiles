local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
keymap.set(
  'n',
  '<leader>q',
  vim.diagnostic.setloclist,
  { desc = 'Open diagnostic [Q]uickfix list' }
)

-- Exit terminal mode in the builtin terminal with a shortcut
keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Split windows
keymap.set('n', 'ss', ':vsplit<Return>', opts)
keymap.set('n', 'sv', ':split<Return>', opts)

-- Tabs
keymap.set('n', 'te', ':tabedit', opts)
keymap.set('n', '<tab>', ':tabnext<Return>', opts)
keymap.set('n', '<s-tab>', ':tabprev<Return>', opts)

-- IncRename
vim.keymap.set('n', '<leader>cr', function()
  return ':IncRename ' .. vim.fn.expand '<cword>'
end, { desc = 'LSP Rename', expr = true })

-- Borderless lazygit
-- keymap.set('n', '<leader>gg', function()
--   Snacks.terminal.get(
--     'lazygit',
--     { esc_esc = false, ctrl_hjkl = false, border = 'none' }
--   )
-- end, { desc = 'Lazygit (root dir)' })

-- Show notifications history
keymap.set('n', '<leader>n', function()
  Snacks.notifier.show_history()
end, { desc = '[N]otifications' })

-- Buffers
keymap.set('n', '<S-h>', '<cmd>bprevious<cr>', { desc = 'Prev Buffer' })
keymap.set('n', '<S-l>', '<cmd>bnext<cr>', { desc = 'Next Buffer' })
keymap.set('n', '[b', '<cmd>bprevious<cr>', { desc = 'Prev Buffer' })
keymap.set('n', ']b', '<cmd>bnext<cr>', { desc = 'Next Buffer' })
keymap.set(
  'n',
  '<leader>bb',
  '<cmd>e #<cr>',
  { desc = 'Switch to Other Buffer' }
)
keymap.set(
  'n',
  '<leader>`',
  '<cmd>e #<cr>',
  { desc = 'Switch to Other Buffer' }
)
keymap.set('n', '<leader>bd', function()
  Snacks.bufdelete()
end, { desc = 'Delete Buffer' })
keymap.set('n', '<leader>bo', function()
  Snacks.bufdelete.other()
end, { desc = 'Delete Other Buffers' })
keymap.set(
  'n',
  '<leader>bD',
  '<cmd>:bd<cr>',
  { desc = 'Delete Buffer and Window' }
)

-- Highlight when yanking (copying) text
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup(
    'kickstart-highlight-yank',
    { clear = true }
  ),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Better indenting
keymap.set('v', '<', '<gv')
keymap.set('v', '>', '>gv')
