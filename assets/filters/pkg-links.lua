-- Turn [pkg]{.pkg} spans into links to the package's documentation.
--
-- * openair-project packages link to their pkgdown sites.
-- * Packages listed in `custom` link to the given URL.
-- * Everything else links to its CRAN page.
--
-- Spans inside headings or existing links are left untouched, as are spans
-- that already contain a link (e.g. [[pkg](url)]{.pkg}).

local openair_project = {
  openair = true,
  openairmaps = true,
  worldmet = true,
  deweather = true,
  euroaq = true
}

local custom = {
  R = "https://www.r-project.org/",
  quarto = "https://quarto.org/",
  rmarkdown = "https://rmarkdown.rstudio.com/",
  splitr = "https://github.com/rich-iannone/splitr"
}

local function pkg_url(name)
  if custom[name] then
    return custom[name]
  end
  if openair_project[name] then
    return "https://openair-project.github.io/" .. name .. "/"
  end
  return "https://CRAN.R-project.org/package=" .. name
end

local function has_link(el)
  local found = false
  el:walk({ Link = function() found = true end })
  return found
end

return {
  {
    traverse = "topdown",
    Header = function(el) return el, false end,
    Link = function(el) return el, false end,
    Span = function(el)
      if not el.classes:includes("pkg") or has_link(el) then
        return nil
      end
      local name = pandoc.utils.stringify(el.content)
      el.content = { pandoc.Link(el.content, pkg_url(name)) }
      return el, false
    end
  }
}
