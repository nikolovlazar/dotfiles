return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons', 'catppuccin/nvim' },
    config = function()
      -- Eviline config for lualine
      -- Author: shadmansaleh
      -- Credit: glepnir
      local lualine = require 'lualine'

      require('plugins.editor.lualine-theme').apply_highlights()

      local conditions = {
        buffer_not_empty = function()
          return vim.fn.empty(vim.fn.expand '%:t') ~= 1
        end,
        hide_in_width = function()
          return vim.fn.winwidth(0) > 80
        end,
        check_git_workspace = function()
          local filepath = vim.fn.expand '%:p:h'
          local gitdir = vim.fn.finddir('.git', filepath .. ';')
          return gitdir and #gitdir > 0 and #gitdir < #filepath
        end,
      }

      -- Config
      local config = {
        options = {
          -- Disable sections and component separators
          component_separators = '',
          section_separators = '',
          theme = {
            -- We are going to use lualine_c an lualine_x as left and
            -- right section. Both are highlighted by c theme .  So we
            -- are just setting default looks o statusline
            normal = { c = 'LualineNormalC' },
            inactive = { c = 'LualineInactiveC' },
          },
        },
        sections = {
          -- these are to remove the defaults
          lualine_a = {},
          lualine_b = {},
          lualine_y = {},
          lualine_z = {},
          -- These will be filled later
          lualine_c = {},
          lualine_x = {},
        },
        inactive_sections = {
          -- these are to remove the defaults
          lualine_a = {},
          lualine_b = {},
          lualine_y = {},
          lualine_z = {},
          lualine_c = {},
          lualine_x = {},
        },
      }

      -- Inserts a component in lualine_c at left section
      local function ins_left(component)
        table.insert(config.sections.lualine_c, component)
      end

      -- Inserts a component in lualine_x at right section
      local function ins_right(component)
        table.insert(config.sections.lualine_x, component)
      end

      ins_left {
        -- mode component
        function()
          return ''
        end,
        color = function()
          local dynamic_colors =
            require('plugins.editor.lualine-theme').build_theme()
          -- auto change color according to neovims mode
          local mode_color = {
            n = dynamic_colors.red,
            i = dynamic_colors.green,
            v = dynamic_colors.blue,
            [''] = dynamic_colors.blue,
            V = dynamic_colors.blue,
            c = dynamic_colors.magenta,
            no = dynamic_colors.red,
            s = dynamic_colors.orange,
            S = dynamic_colors.orange,
            [''] = dynamic_colors.orange,
            ic = dynamic_colors.yellow,
            R = dynamic_colors.violet,
            Rv = dynamic_colors.violet,
            cv = dynamic_colors.red,
            ce = dynamic_colors.red,
            r = dynamic_colors.cyan,
            rm = dynamic_colors.cyan,
            ['r?'] = dynamic_colors.cyan,
            ['!'] = dynamic_colors.red,
            t = dynamic_colors.red,
          }
          return { bg = dynamic_colors.bg, fg = mode_color[vim.fn.mode()] }
        end,
        padding = { right = 1 },
      }

      ins_left {
        'filename',
        cond = conditions.buffer_not_empty,
        color = 'LualineFilename',
      }

      ins_left {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = { error = ' ', warn = ' ', info = ' ' },
        diagnostics_color = {
          error = 'LualineDiagnosticsError',
          warn = 'LualineDiagnosticsWarn',
          info = 'LualineDiagnosticsInfo',
        },
      }

      -- Insert mid section. You can make any number of sections in neovim :)
      -- for lualine it's any number greater then 2
      ins_left {
        function()
          return '%='
        end,
      }

      ins_right {
        function()
          local ok, laravel_version = pcall(function()
            return Laravel.app('status'):get 'laravel'
          end)
          if ok then
            return laravel_version
          end
        end,
        icon = { '', color = { fg = '#F55247' } },
        cond = function()
          local ok, has_laravel_versions = pcall(function()
            return Laravel.app('status'):has 'laravel'
          end)
          return ok and has_laravel_versions
        end,
      }

      ins_right {
        function()
          local msg = ''
          local buf_ft = vim.api.nvim_get_option_value('filetype', { buf = 0 })
          local clients = vim.lsp.get_clients()
          if next(clients) == nil then
            return msg
          end
          for _, client in ipairs(clients) do
            local filetypes = client.config.filetypes
            if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
              return client.name
            end
          end
          return msg
        end,
        icon = '󰧑',
        color = 'LualineLsp',
      }

      -- Add components to right sections
      ins_right {
        'branch',
        icon = '',
        color = 'LualineBranch',
      }

      ins_right {
        'diff',
        symbols = { added = ' ', modified = ' ', removed = ' ' },
        diff_color = {
          added = 'LualineDiffAdded',
          modified = 'LualineDiffModified',
          removed = 'LualineDiffRemoved',
        },
        cond = conditions.hide_in_width,
      }

      ins_right {
        function()
          return os.date '%H:%M'
        end,
        icon = '',
        color = 'LualineDiagnosticsWarn',
      }

      lualine.setup(config)
    end,
  },
}
