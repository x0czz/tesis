-- bib-aparte.lua
-- Procesa las citas de un bloque ::: {.bib-aparte bibliography="..."} con su propia
-- bibliografía y deja la lista de referencias dentro del mismo bloque, en el div
-- #lista-aparte. Las citas del bloque se convierten en texto antes de que corra el
-- citeproc del libro, de modo que no aparecen en la página general de Referencias.

local function meta_para(div, doc_meta)
  local m = pandoc.Meta({})
  m.bibliography = pandoc.MetaList({ pandoc.MetaString(div.attributes["bibliography"]) })
  m.csl = pandoc.MetaString(div.attributes["csl"] or "refs/apa.csl")
  m.lang = doc_meta.lang or pandoc.MetaString("es")
  m["link-citations"] = true
  return m
end

local function procesar(div, doc_meta)
  local blocks = div.content:walk({
    Div = function(d) if d.identifier == "lista-aparte" then d.identifier = "refs" end return d end
  })
  local out = pandoc.utils.citeproc(pandoc.Pandoc(blocks, meta_para(div, doc_meta)))
  local res = out.blocks:walk({
    -- Las citas quedan como texto (sin la clase "citation"), para que el libro no las recoja
    Cite = function(c) return pandoc.Span(c.content, pandoc.Attr("", { "cita-aparte" })) end,
    Div = function(d)
      if d.identifier == "refs" then d.identifier = "lista-aparte" end
      return d
    end
  })
  div.content = res
  div.attributes["bibliography"] = nil
  div.attributes["csl"] = nil
  return div
end

function Pandoc(doc)
  doc.blocks = doc.blocks:walk({
    Div = function(div)
      if div.classes:includes("bib-aparte") and div.attributes["bibliography"] then
        return procesar(div, doc.meta)
      end
    end
  })
  return doc
end
