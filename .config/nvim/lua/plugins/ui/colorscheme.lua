return {
  {
    'ricardoraposo/nightwolf.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd [[colorscheme nightwolf]]
    end,
  },
  -- {
  --   'scottmckendry/cyberdream.nvim',
  --   lazy = false,
  --   priority = 1000,
  --   config = function()
  --     require('cyberdream').setup {
  --       transparent = true,
  --       italic_comments = true,
  --       borderless_telescope = false,
  --     }
  --     vim.cmd [[colorscheme cyberdream]]
  --   end,
  -- },
}
