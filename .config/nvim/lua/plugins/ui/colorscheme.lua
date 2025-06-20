return {
  {
    'eliseshaffer/darklight.nvim',
    config = function()
      require('darklight').setup {
        mode = 'custom',
        light_mode_callback = function()
          vim.o.background = 'light'
        end,
        dark_mode_callback = function()
          vim.o.background = 'dark'
        end,
      }
    end,
  },
  {
    'scottmckendry/cyberdream.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('cyberdream').setup {
        variant = 'auto',
        transparent = true,
        italic_comments = true,
        borderless_telescope = false,
      }
      vim.cmd [[colorscheme cyberdream]]
    end,
  },
}
