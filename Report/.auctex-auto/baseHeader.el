(TeX-add-style-hook
 "baseHeader"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("graphicx" "pdftex") ("ku-frontpage" "babel" "en") ("appendix" "toc" "page") ("geometry" "a4paper" "hmargin={1.5in, 1.5in}" "vmargin={1.5in, 1.5in}")))
   (TeX-run-style-hooks
    "inputenc"
    "latexsym"
    "amssymb"
    "amsmath"
    "svg"
    "subfig"
    "graphicx"
    "ku-frontpage"
    "float"
    "caption"
    "appendix"
    "fancyvrb"
    "url"
    "tabularx"
    "listings"
    "geometry"
    "minted"
    "mdframed"
    "sourcecodepro"
    "booktabs"
    "titlesec"
    "syntax")
   (TeX-add-symbols
    "phantomsection"
    "listofillustrations"
    "summaryname")
   (LaTeX-add-environments
    "changemargin"
    "Abstract"))
 :latex)

