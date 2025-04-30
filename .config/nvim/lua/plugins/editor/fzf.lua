local kind_filter = {
  default = {
    'Class',
    'Constructor',
    'Enum',
    'Field',
    'Function',
    'Interface',
    'Method',
    'Module',
    'Namespace',
    'Package',
    'Property',
    'Struct',
    'Trait',
  },
  markdown = false,
  help = false,
  lua = {
    'Class',
    'Constructor',
    'Enum',
    'Field',
    'Function',
    'Interface',
    'Method',
    'Module',
    'Namespace',
    -- "Package", -- remove package since luals uses it for control flow structures
    'Property',
    'Struct',
    'Trait',
  },
}

local get_kind_filter = function(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local ft = vim.bo[buf].filetype

  if kind_filter[ft] == false then
    return
  end

  if type(kind_filter[ft]) == 'table' then
    return kind_filter[ft]
  end

  ---@diagnostic disable-next-line: return-type-mismatch
  return type(kind_filter) == 'table'
      and type(kind_filter.default) == 'table'
      and kind_filter.default
    or nil
end

local symbols_filter = function(entry, ctx)
  if ctx.symbols_filter == nil then
    ctx.symbols_filter = get_kind_filter(ctx.bufnr) or false
  end
  if ctx.symbols_filter == false then
    return true
  end
  return vim.tbl_contains(ctx.symbols_filter, entry.kind)
end

return {
  {
    'ibhagwan/fzf-lua',
    dependencies = { 'echasnovski/mini.icons' },
    opts = {
      files = {
        fd_opts = '--color=never --type f --hidden --follow',
      },
      grep = {
        rg_opts = '--column --line-number --hidden',
      },
    },
    keys = {
      { '<c-j>', '<c-j>', ft = 'fzf', mode = 't', nowait = true },
      { '<c-k>', '<c-k>', ft = 'fzf', mode = 't', nowait = true },
      {
        '<leader>,',
        '<cmd>FzfLua buffers sort_mru=true sort_lastused=true<cr>',
        desc = 'Switch Buffer',
      },

      -- find
      {
        '<leader>fb',
        '<cmd>FzfLua buffers sort_mru=true sort_lastused=true<cr>',
        desc = 'Buffers',
      },
      { '<leader>ff', '<cmd>FzfLua files<cr>', desc = 'Files' },
      { '<leader><space>', '<cmd>FzfLua files<cr>', desc = 'Find Files' },
      { '<leader>fg', '<cmd>FzfLua git_files<cr>', desc = 'Git-files' },

      -- search
      { '<leader>s"', '<cmd>FzfLua registers<cr>', desc = 'Registers' },
      { '<leader>sa', '<cmd>FzfLua autocmds<cr>', desc = 'Auto Commands' },
      { '<leader>sb', '<cmd>FzfLua grep_curbuf<cr>', desc = 'Buffer' },
      {
        '<leader>sc',
        '<cmd>FzfLua command_history<cr>',
        desc = 'Command History',
      },
      { '<leader>sC', '<cmd>FzfLua commands<cr>', desc = 'Commands' },
      {
        '<leader>sd',
        '<cmd>FzfLua diagnostics_document<cr>',
        desc = 'Document Diagnostics',
      },
      {
        '<leader>sD',
        '<cmd>FzfLua diagnostics_workspace<cr>',
        desc = 'Workspace Diagnostics',
      },
      { '<leader>sg', '<cmd>FzfLua live_grep_native<cr>', desc = 'Grep' },
      { '<leader>sh', '<cmd>FzfLua help_tags<cr>', desc = 'Help Pages' },
      { '<leader>sH', '<cmd>FzfLua highlights<cr>', desc = 'Highlight Groups' },
      { '<leader>sj', '<cmd>FzfLua jumps<cr>', desc = 'Jumplist' },
      { '<leader>sk', '<cmd>FzfLua keymaps<cr>', desc = 'Key Maps' },
      { '<leader>sl', '<cmd>FzfLua loclist<cr>', desc = 'Location List' },
      { '<leader>sM', '<cmd>FzfLua man_pages<cr>', desc = 'Man Pages' },
      { '<leader>sm', '<cmd>FzfLua marks<cr>', desc = 'Jump to Mark' },
      { '<leader>sR', '<cmd>FzfLua resume<cr>', desc = 'Resume' },
      { '<leader>sq', '<cmd>FzfLua quickfix<cr>', desc = 'Quickfix List' },
      {
        '<leader>ss',
        function()
          require('fzf-lua').lsp_document_symbols {
            regex_filter = symbols_filter,
          }
        end,
        desc = 'Symbol (Document)',
      },
      {
        '<leader>sS',
        function()
          require('fzf-lua').lsp_live_workspace_symbols {
            regex_filter = symbols_filter,
          }
        end,
        desc = 'Symbol (Workspace)',
      },
    },
  },
  {
    'neovim/nvim-lspconfig',
    opts = function()
      -- stylua: ignore
      vim.keymap.set('n', 'gd', '<cmd>FzfLua lsp_definitions     jump_to_single_result=true silent=true ignore_current_line=true<cr>', { desc = "Goto Definition" })
      -- stylua: ignore
      vim.keymap.set('n', 'gr', '<cmd>FzfLua lsp_references      jump_to_single_result=true silent=true ignore_current_line=true<cr>', { desc = "References", nowait = true })
      -- stylua: ignore
      vim.keymap.set('n', 'gI', '<cmd>FzfLua lsp_implementations jump_to_single_result=true silent=true ignore_current_line=true<cr>', { desc = "Goto Implementation" })
      -- stylua: ignore
      vim.keymap.set('n', 'gy', '<cmd>FzfLua lsp_typedefs        jump_to_single_result=true silent=true ignore_current_line=true<cr>', { desc = "Goto Type Definitions" })
    end,
  },
}
