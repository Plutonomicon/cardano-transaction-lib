function Pandoc (doc)
  local already_set = false;
  doc.blocks:walk {
    Header = function (h)
      -- use top-level heading as title, unless the doc
      -- already has a title
      if h.level == 1 and not already_set then
        doc.meta.title = h.content
        already_set = true
        return {}  -- remove this heading from the body
      end
    end
  }
  return doc
end
