return {
  {
    'catppuccin/nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('catppuccin').setup {
        flavour = 'mocha',
      }

      vim.cmd [[colorscheme catppuccin]]
    end,
  },
}
