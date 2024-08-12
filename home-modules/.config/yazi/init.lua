-- overwrites Current:render and Status:render
require("relative-motions"):setup({ show_numbers = "relative_absolute", show_motion = "true" })
require("starship"):setup()

function Manager:render(area)
  local chunks = self:layout(area)

  local bar = function(c, x, y)
    x, y = math.max(0, x), math.max(0, y)
    return ui.Bar(
      ui.Rect { x = x,
        y = y,
        w = ya.clamp(0, area.w - x, 1),
        h = math.min(1, area.h)
      }, ui.Bar.TOP):symbol(c)
  end

  -- Modified for full borders
  return ya.flat {
    -- Borders
    ui.Border(area, ui.Border.ALL):type(ui.Border.ROUNDED),
    ui.Bar(chunks[1], ui.Bar.RIGHT),
    ui.Bar(chunks[3], ui.Bar.LEFT),

    bar("┬", chunks[1].right - 1, chunks[1].y),
    bar("┴", chunks[1].right - 1, chunks[1].bottom - 1),
    bar("┬", chunks[2].right, chunks[2].y),
    bar("┴", chunks[2].right, chunks[1].bottom - 1),

    -- Parent
    Parent:render(chunks[1]:padding(ui.Padding.xy(1))),
    -- Current
    Current:render(chunks[2]:padding(ui.Padding.y(1))),
    -- Preview
    Preview:render(chunks[3]:padding(ui.Padding.xy(1))),
  }
end

function Status:name()
  local h = cx.active.current.hovered
  if not h then
    return ui.Span("")
  end

  -- Modified to show symlink in status bar
  local linked = ""
  if h.link_to ~= nil then
    linked = " -> " .. tostring(h.link_to)
  end
  return ui.Span(" " .. h.name .. linked)
end

-- Added to help show user/group in status bar
function Status:owner()
  local h = cx.active.current.hovered
  if h == nil or ya.target_family() ~= "unix" then
    return ui.Line {}
  end

  return ui.Line {
    ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
    ui.Span(":"),
    ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
    ui.Span(" "),
  }
end

function Status:render(area)
  self.area = area

  local left = ui.Line { self:mode(), self:size(), self:name() }
  -- Modified to show user/group of files in status bar
  -- Also modified to show current motion for relative-motions.yazi
  local right = ui.Line { self:motion(), self:owner(), self:permissions(), self:percentage(), self:position() }
  return {
    ui.Paragraph(area, { left }),
    ui.Paragraph(area, { right }):align(ui.Paragraph.RIGHT),
    table.unpack(Progress:render(area, right:width())),
  }
end
