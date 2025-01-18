local prefix = '<leader>a'

return {
  {
    'olimorris/codecompanion.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
    },
    cmd = {
      'CodeCompanion',
      'CodeCompanionActions',
      'CodeCompanionToggle',
      'CodeCompanionAdd',
      'CodeCompanionChat',
    },
    keys = {
      {
        prefix .. 'a',
        '<cmd>CodeCompanionActions<cr>',
        mode = { 'n', 'v' },
        desc = 'Action Palette',
      },
      {
        prefix .. 'c',
        '<cmd>CodeCompanionChat<cr>',
        mode = { 'n', 'v' },
        desc = 'New Chat',
      },
      {
        prefix .. 'A',
        '<cmd>CodeCompanionAdd<cr>',
        mode = 'v',
        desc = 'Add Code',
      },
      {
        prefix .. 'i',
        '<cmd>CodeCompanion<cr>',
        mode = 'n',
        desc = 'Inline Prompt',
      },
      {
        prefix .. 'C',
        '<cmd>CodeCompanionToggle<cr>',
        mode = 'n',
        desc = 'Toggle Chat',
      },
    },
    opts = {
      strategies = {
        chat = {
          adapter = 'copilot',
        },
        inline = {
          adapter = 'copilot',
        },
      },
      opts = {
        log_level = 'DEBUG',
      },
    },
  },
}
