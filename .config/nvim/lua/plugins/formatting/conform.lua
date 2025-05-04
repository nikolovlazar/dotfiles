return {
  {
    'stevearc/conform.nvim',
    opts = {
      formatters_by_ft = {
        lua = { 'stylua' },
        php = { 'php_cs_fixer' },
        blade = { 'blade-formatter' },
      },
    },
  },
}
