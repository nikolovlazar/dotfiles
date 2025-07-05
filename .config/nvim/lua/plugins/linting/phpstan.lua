local function get_cwd()
  return vim.fn.getcwd()
end

return {
  name = 'phpstan',
  cmd = './vendor/bin/phpstan',
  stdin = false,
  append_fname = true,
  args = {
    '--memory-limit=2G',
    'analyse',
    '--error-format=raw',
    '--no-progress',
  },
  stream = 'both',
  ignore_exitcode = true,
  cwd = get_cwd(),
  parser = require('lint.parser').from_pattern(
    '^.+:(%d+):(.+)$',
    { 'lnum', 'message' },
    nil,
    {
      source = 'phpstan',
      severity = vim.diagnostic.severity.WARN,
    }
  ),
}
