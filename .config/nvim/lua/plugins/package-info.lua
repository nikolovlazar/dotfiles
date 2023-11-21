return {
  {
    "vuki656/package-info.nvim",
    config = function()
      local colors = require("catppuccin.palettes").get_palette("mocha")

      require("package-info").setup({
        package_manager = "pnpm",
        colors = {
          outdated = colors.peach,
        },
        hide_up_to_date = true,
      })
    end,
    dependencies = { "MunifTanjim/nui.nvim" },
  },
}
