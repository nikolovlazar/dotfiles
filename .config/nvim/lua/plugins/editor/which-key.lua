return {
  {
    'folke/which-key.nvim',
    event = 'VimEnter',
    opts = {
      preset = 'helix',
      delay = 0,
      icons = {
        mappings = vim.g.have_nerd_font,
        keys = vim.g.have_nerd_font and {} or {
          Up = '<Up> ',
          Down = '<Down> ',
          Left = '<Left> ',
          Right = '<Right> ',
          C = '<C-…> ',
          M = '<M-…> ',
          D = '<D-…> ',
          S = '<S-…> ',
          CR = '<CR> ',
          Esc = '<Esc> ',
          ScrollWheelDown = '<ScrollWheelDown> ',
          ScrollWheelUp = '<ScrollWheelUp> ',
          NL = '<NL> ',
          BS = '<BS> ',
          Space = '<Space> ',
          Tab = '<Tab> ',
          F1 = '<F1>',
          F2 = '<F2>',
          F3 = '<F3>',
          F4 = '<F4>',
          F5 = '<F5>',
          F6 = '<F6>',
          F7 = '<F7>',
          F8 = '<F8>',
          F9 = '<F9>',
          F10 = '<F10>',
          F11 = '<F11>',
          F12 = '<F12>',
        },
      },

      -- Document existing key chains
      spec = {
        {
          '<leader>a',
          group = 'AI',
          icon = { icon = '󱚦', color = 'cyan' },
        },
        {
          '<leader>b',
          group = 'Buffers',
          icon = { icon = '', color = 'yellow' },
        },
        {
          '<leader>c',
          group = 'Code',
          mode = { 'n', 'x' },
          icon = { icon = '', color = 'green' },
        },
        {
          '<leader>d',
          group = 'Debug',
          icon = { icon = '', color = 'red' },
        },
        {
          '<leader>f',
          group = 'Find',
          icon = { icon = '󰈞', color = 'yellow' },
        },
        {
          '<leader>g',
          group = 'Git',
          icon = { icon = '󰊢', color = 'orange' },
        },
        {
          '<leader>gb',
          group = 'Buff/Blame',
          icon = { icon = '󰊢', color = 'orange' },
        },
        {
          '<leader>gd',
          group = 'Diff/Deleted',
          icon = { icon = '󰊢', color = 'orange' },
        },
        {
          '<leader>gh',
          group = 'Hunks',
          icon = { icon = '󰊢', color = 'orange' },
        },
        {
          '<leader>o',
          group = 'Overseer',
          icon = { icon = '', color = 'red' },
        },
        {
          '<leader>s',
          group = 'Search',
          icon = { icon = '', color = 'yellow' },
        },
        {
          '<leader>t',
          group = 'Test',
          icon = { icon = '󰙨', color = 'purple' },
        },
        {
          'gz',
          group = 'Surround',
          mode = { 'n', 'v' },
          icon = { icon = '', color = 'red' },
        },
      },
    },
  },
}
-- vim: ts=2 sts=2 sw=2 et
