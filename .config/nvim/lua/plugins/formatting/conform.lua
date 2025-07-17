return {
  {
    'stevearc/conform.nvim',
    opts = {
      formatters_by_ft = {
        lua = { 'stylua' },
        php = { 'pint' },
        blade = { 'blade-formatter' },
        javascript = { 'biome' },
        typescript = { 'biome' },
      },
      formatters = {
        pint = {
          command = 'vendor/bin/pint',
          args = { '$FILENAME' },
          stdin = false,
        },
        biome = {
          require_cwd = true,
        },
      },
    },
  },
}
