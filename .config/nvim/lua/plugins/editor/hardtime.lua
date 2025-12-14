return {
  {
    'm4xshen/hardtime.nvim',
    lazy = false,
    dependencies = { 'MunifTanjim/nui.nvim' },
    opts = {
      restricted_keys = {
        ['h'] = false,
        ['j'] = false,
        ['k'] = false,
        ['l'] = false,
      },
    },
  },
}
