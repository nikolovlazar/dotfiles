return {
  {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local lint = require 'lint'

      lint.linters_by_ft = lint.linters_by_ft or {}
      lint.linters_by_ft['php'] = { 'phpstan' }

      lint.linters = lint.linters or {}
      lint.linters.phpstan = require 'plugins.linting.phpstan'

      local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })
      vim.api.nvim_create_autocmd(
        { 'BufEnter', 'BufWritePost', 'InsertLeave' },
        {
          group = lint_augroup,
          callback = function()
            if vim.opt_local.modifiable:get() then
              lint.try_lint()
            end
          end,
        }
      )
    end,
  },
}
