return {
  {
    'stevearc/overseer.nvim',
    opts = {
      task_list = {
        direction = 'left',
        bindings = {
          ['<C-h>'] = false,
          ['<C-j>'] = false,
          ['<C-k>'] = false,
          ['<C-l>'] = false,
          ['L'] = 'IncreaseDetail',
          ['H'] = 'DecreaseDetail',
          ['<PageUp>'] = 'ScrollOutputUp',
          ['<PageDown>'] = 'ScrollOutputDown',
        },
      },
    },
    keys = {
      {
        '<leader>ot',
        '<cmd>OverseerToggle<CR>',
        desc = 'Toggle Overseer Task List',
      },
      { '<leader>or', '<cmd>OverseerRun<CR>', desc = 'Run Overseer Task' },
      {
        '<leader>ol',
        '<cmd>OverseerRunCmd<CR>',
        desc = 'Run Command in Overseer',
      },
      {
        '<leader>oq',
        '<cmd>OverseerQuickAction<CR>',
        desc = 'Quick Action for Overseer Task',
      },
      {
        '<leader>oa',
        '<cmd>OverseerTaskAction<CR>',
        desc = 'Select and Act on Overseer Task',
      },
      {
        '<leader>oc',
        '<cmd>OverseerClearCache<CR>',
        desc = 'Clear Overseer Task Cache',
      },
      {
        '<leader>os',
        '<cmd>OverseerSaveBundle<CR>',
        desc = 'Save Overseer Task Bundle',
      },
      {
        '<leader>ob',
        '<cmd>OverseerLoadBundle<CR>',
        desc = 'Load Overseer Task Bundle',
      },
    },
  },
}
