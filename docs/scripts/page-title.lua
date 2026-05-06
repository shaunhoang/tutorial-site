local site_title = "Small Area Estimation for Urban Analytics"

function Meta(meta)
  local page_title = pandoc.utils.stringify(meta.title or "")

  if page_title ~= "" and page_title ~= site_title then
    meta.pagetitle = page_title .. " | " .. site_title
  elseif page_title == site_title then
    meta.pagetitle = site_title
  end

  return meta
end
