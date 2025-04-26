local model_info_provider = {}

function model_info_provider:boot(app)
  local model_info_view = {}

  function model_info_view:get(model)
    local base_indent = '  '
    local virt_lines = {}

    table.insert(virt_lines, {
      { '󰆧 Model', 'Comment' },
    })

    if model.database then
      table.insert(virt_lines, {
        { base_indent .. 'Database: ', 'Comment' },
        { model.database, '@variable' },
      })
    end

    if model.table then
      table.insert(virt_lines, {
        { base_indent .. 'Table: ', 'Comment' },
        { model.table, '@variable' },
      })
    end

    if model.attributes then
      table.insert(virt_lines, {
        { base_indent .. 'Attributes:', 'Comment' },
      })

      -- Find max lengths
      local max_field_len = 0
      local max_type_len = 0
      for _, attr in ipairs(model.attributes) do
        if attr.name and #attr.name > max_field_len then
          max_field_len = #attr.name
        end
        if attr.type and #attr.type > max_type_len then
          max_type_len = #attr.type
        end
      end

      for _, attr in ipairs(model.attributes) do
        local field = attr.name or ''
        local type_ = attr.type or 'null'

        local field_padding = string.rep(' ', max_field_len - #field)
        local type_padding = string.rep(' ', max_type_len - #type_)

        local line = {
          { base_indent .. '  ' .. field .. field_padding .. '  ', 'Normal' },
          { type_ .. type_padding, 'Comment' },
        }

        if attr.cast then
          table.insert(line, { ' ➔ ', 'Comment' })
          table.insert(line, { attr.cast, '@type' })
        end

        table.insert(virt_lines, line)
      end
    end

    return {
      virt_lines = virt_lines,
      virt_lines_above = true,
    }
  end

  app:instance('model_info_view', model_info_view)
end

return model_info_provider
