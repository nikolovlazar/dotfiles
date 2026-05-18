return {
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = {
        "eslint-lsp",
        "hadolint",
        "shfmt",
        "stylua",
        "shellcheck",
      }
    end,
  },
}
