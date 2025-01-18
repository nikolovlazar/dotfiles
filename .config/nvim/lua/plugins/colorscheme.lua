return {
  {
    'scottmckendry/cyberdream.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('cyberdream').setup {
        transparent = true,
        italic_comments = true,
        borderless_telescope = false,
      }
      vim.cmd [[colorscheme cyberdream]]
    end,
  },

  -- modicator (auto color line number based on vim mode)
  {
    'mawkler/modicator.nvim',
    dependencies = 'scottmckendry/cyberdream.nvim',
    init = function()
      -- These are required for Modicator to work
      vim.o.cursorline = false
      vim.o.number = true
      vim.o.termguicolors = true
    end,
    opts = {},
  },
}
