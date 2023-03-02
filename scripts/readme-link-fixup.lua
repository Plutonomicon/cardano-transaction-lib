function Link(el)
  el.target = string.gsub(el.target, "./doc/", "")
  return el
end
