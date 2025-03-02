return {
  {
    'ricardoraposo/nightwolf.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd [[colorscheme nightwolf]]
      vim.api.nvim_set_hl(0, 'FoldColumn', { fg = '#9696ff' })
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
