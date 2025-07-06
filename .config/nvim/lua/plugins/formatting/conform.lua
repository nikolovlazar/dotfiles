return {
  {
    'stevearc/conform.nvim',
    opts = {
      formatters_by_ft = {
        lua = { 'stylua' },
        php = { 'pint' },
        blade = { 'blade-formatter' },
      },
      formatters = {
        pint = {
          command = 'vendor/bin/pint',
          args = { '$FILENAME' },
          stdin = false,
        },
      },
    },
  },
}
