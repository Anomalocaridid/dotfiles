require("full-border"):setup()
require("relative-motions"):setup { show_numbers = "relative_absolute", show_motion = "true" }
require("starship"):setup()

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

-- Add user/group of files in status bar
Status:children_add(function()
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
end, 500, Status.RIGHT)
