return {
  {
    'stevearc/conform.nvim',
    optional = true,
    opts = {
      formatters = {
        ['markdown-toc'] = {
          condition = function(_, ctx)
            for _, line in
              ipairs(vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false))
            do
              if line:find '<!%-%- toc %-%->' then
                return true
              end
            end
          end,
        },
      },
      formatters_by_ft = {
        ['markdown'] = { 'prettier', 'markdown-toc' },
        ['markdown.mdx'] = { 'prettier', 'markdown-toc' },
      },
    },
  },
  {
    'williamboman/mason.nvim',
    opts = { ensure_installed = { 'markdown-toc' } },
  },
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        marksman = {},
      },
    },
  },
  {
    'MeanderingProgrammer/render-markdown.nvim',
    opts = {
      file_types = { 'markdown', 'markdown.mdx' },
      code = {
        sign = false,
        width = 'block',
        right_pad = 1,
      },
      heading = {
        sign = false,
        icons = {},
      },
      checkbox = {
        enabled = false,
      },
      latex = {
        enabled = false,
      },
    },
    ft = { 'markdown', 'norg', 'rmd', 'org', 'markdown.mdx' },
    config = function(_, opts)
      require('render-markdown').setup(opts)
    end,
  },
}
