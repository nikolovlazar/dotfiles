return {
  "nvim-neo-tree/neo-tree.nvim",
  opts = {
    window = {
      position = "right",
      width = 30,
      mappings = {
        ["Y"] = "none",
      },
    },
    filesystem = {
      filtered_items = {
        hide_dotfiles = false,
        hide_by_name = {
          ".git",
          ".DS_Store",
        },
        always_show = {
          ".env",
        },
      },
    },
  },
}
