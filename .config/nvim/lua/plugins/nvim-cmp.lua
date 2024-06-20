return {
  {
    "hrsh7th/nvim-cmp",
    opts = function(_, opts)
      opts.completion.autocomplete = false
      opts.mapping["<CR>"] = nil
    end,
  },
}
