-- TypeScript / JavaScript via vtsls. Inlay hints are configured here but only
-- render when toggled on with `<leader>th` (they are off by default, so no
-- idle CPU cost). The `@astrojs/ts-plugin` global plugin gives TS-aware Astro.
local inlay_hints = {
  enumMemberValues = { enabled = true },
  functionLikeReturnTypes = { enabled = true },
  parameterNames = { enabled = 'literals' },
  parameterTypes = { enabled = true },
  propertyDeclarationTypes = { enabled = true },
  variableTypes = { enabled = false },
}

local language_settings = {
  updateImportsOnFileMove = { enabled = 'always' },
  suggest = { completeFunctionCalls = true },
  inlayHints = inlay_hints,
}

return {
  settings = {
    complete_function_calls = true,
    vtsls = {
      enableMoveToFileCodeAction = true,
      autoUseWorkspaceTsdk = true,
      experimental = {
        maxInlayHintLength = 30,
        completion = {
          enableServerSideFuzzyMatch = true,
        },
      },
      tsserver = {
        globalPlugins = {
          {
            name = '@astrojs/ts-plugin',
            location = vim.env.MASON
              .. '/packages/astro-language-server/node_modules/@astrojs/ts-plugin',
            enableForWorkspaceTypeScriptVersions = true,
          },
        },
      },
    },
    typescript = language_settings,
    javascript = language_settings,
  },
}
