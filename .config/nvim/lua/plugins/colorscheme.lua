return {
  {
    "nyoom-engineering/oxocarbon.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd("colorscheme oxocarbon")

      local oxocarbon = require("oxocarbon").oxocarbon

      -- changing terminal_color_4 fixes the contrast issue for LazyGit
      vim.g["terminal_color_4"] = oxocarbon.base03

      -- nvim-cmp color overrides
      vim.api.nvim_set_hl(0, "CmpItemKindInterface", { bg = oxocarbon.base01, fg = oxocarbon.base08 })
      vim.api.nvim_set_hl(0, "CmpItemKindColor", { bg = oxocarbon.base01, fg = oxocarbon.base08 })
      vim.api.nvim_set_hl(0, "CmpItemKindTypeParameter", { bg = oxocarbon.base01, fg = oxocarbon.base08 })
      vim.api.nvim_set_hl(0, "CmpItemKindText", { bg = oxocarbon.base01, fg = oxocarbon.base09 })
      vim.api.nvim_set_hl(0, "CmpItemKindEnum", { bg = oxocarbon.base01, fg = oxocarbon.base09 })
      vim.api.nvim_set_hl(0, "CmpItemKindKeyword", { bg = oxocarbon.base01, fg = oxocarbon.base09 })
      vim.api.nvim_set_hl(0, "CmpItemKindConstant", { bg = oxocarbon.base01, fg = oxocarbon.base10 })
      vim.api.nvim_set_hl(0, "CmpItemKindConstructor", { bg = oxocarbon.base01, fg = oxocarbon.base10 })
      vim.api.nvim_set_hl(0, "CmpItemKindReference", { bg = oxocarbon.base01, fg = oxocarbon.base10 })
      vim.api.nvim_set_hl(0, "CmpItemKindFunction", { bg = oxocarbon.base01, fg = oxocarbon.base11 })
      vim.api.nvim_set_hl(0, "CmpItemKindStruct", { bg = oxocarbon.base01, fg = oxocarbon.base11 })
      vim.api.nvim_set_hl(0, "CmpItemKindClass", { bg = oxocarbon.base01, fg = oxocarbon.base11 })
      vim.api.nvim_set_hl(0, "CmpItemKindModule", { bg = oxocarbon.base01, fg = oxocarbon.base11 })
      vim.api.nvim_set_hl(0, "CmpItemKindOperator", { bg = oxocarbon.base01, fg = oxocarbon.base11 })
      vim.api.nvim_set_hl(0, "CmpItemKindField", { bg = oxocarbon.base01, fg = oxocarbon.base12 })
      vim.api.nvim_set_hl(0, "CmpItemKindProperty", { bg = oxocarbon.base01, fg = oxocarbon.base12 })
      vim.api.nvim_set_hl(0, "CmpItemKindEvent", { bg = oxocarbon.base01, fg = oxocarbon.base12 })
      vim.api.nvim_set_hl(0, "CmpItemKindUnit", { bg = oxocarbon.base01, fg = oxocarbon.base13 })
      vim.api.nvim_set_hl(0, "CmpItemKindSnippet", { bg = oxocarbon.base01, fg = oxocarbon.base13 })
      vim.api.nvim_set_hl(0, "CmpItemKindFolder", { bg = oxocarbon.base01, fg = oxocarbon.base13 })
      vim.api.nvim_set_hl(0, "CmpItemKindVariable", { bg = oxocarbon.base01, fg = oxocarbon.base14 })
      vim.api.nvim_set_hl(0, "CmpItemKindFile", { bg = oxocarbon.base01, fg = oxocarbon.base14 })
      vim.api.nvim_set_hl(0, "CmpItemKindMethod", { bg = oxocarbon.base01, fg = oxocarbon.base15 })
      vim.api.nvim_set_hl(0, "CmpItemKindValue", { bg = oxocarbon.base01, fg = oxocarbon.base15 })
      vim.api.nvim_set_hl(0, "CmpItemKindEnumMember", { bg = oxocarbon.base01, fg = oxocarbon.base15 })

      -- telescope color overrides
      vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = oxocarbon.blend, bg = oxocarbon.blend })
      vim.api.nvim_set_hl(0, "TelescopePromptBorder", { fg = oxocarbon.base02, bg = oxocarbon.blend })
      vim.api.nvim_set_hl(0, "TelescopePromptNormal", { fg = oxocarbon.base05, bg = oxocarbon.blend })
      vim.api.nvim_set_hl(0, "TelescopePromptPrefix", { fg = oxocarbon.base08, bg = oxocarbon.blend })
    end,
  },

  -- modicator (auto color line number based on vim mode)
  {
    "mawkler/modicator.nvim",
    dependencies = "nyoom-engineering/oxocarbon.nvim",
    init = function()
      -- These are required for Modicator to work
      vim.o.cursorline = true
      vim.o.number = true
      vim.o.termguicolors = true
    end,
    opts = {},
  },
}
