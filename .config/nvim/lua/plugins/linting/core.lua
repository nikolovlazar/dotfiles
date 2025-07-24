return {
  {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local lint = require 'lint'

      lint.linters = lint.linters or {}
      lint.linters.phpstan = require 'plugins.linting.phpstan'

      lint.linters_by_ft = lint.linters_by_ft or {}
      lint.linters_by_ft['php'] = { 'phpstan' }

      local function has_file(files)
        for _, file in ipairs(files) do
          if vim.fn.filereadable(vim.fn.getcwd() .. '/' .. file) == 1 then
            return true
          end
        end
        return false
      end

      -- No JS linters configured - using ESLint LSP instead

      local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })
      vim.api.nvim_create_autocmd(
        { 'BufEnter', 'BufWritePost', 'InsertLeave' },
        {
          group = lint_augroup,
          callback = function()
            if not vim.opt_local.modifiable:get() then
              return
            end

            local ft = vim.bo.filetype
            local js_like = {
              javascript = true,
              javascriptreact = true,
              typescript = true,
              ['typescript.tsx'] = true,
              typescriptreact = true,
              json = true,
            }

            -- No JS linting - using ESLint LSP instead

            lint.try_lint(nil, { ignore_errors = true })
          end,
        }
      )
    end,
  },
}
