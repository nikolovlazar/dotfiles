local function get_cwd()
  local current_file = vim.fn.expand '%:p'
  if current_file == '' then
    return vim.fn.getcwd()
  end
  local root = vim.fs.find({ 'phpstan.neon', 'composer.json', 'vendor' }, {
    path = current_file,
    upward = true,
  })[1]
  return root and vim.fs.dirname(root) or vim.fn.getcwd()
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
    '^(.+):(%d+):(.+)$',
    { 'file', 'lnum', 'message' },
    nil,
    {
      source = 'phpstan',
      severity = vim.diagnostic.severity.WARN,
    }
  ),
}
