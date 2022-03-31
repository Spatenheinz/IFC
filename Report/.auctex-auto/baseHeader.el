(TeX-add-style-hook
 "baseHeader"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("ku-frontpage" "english" "science" "titlepage") ("appendix" "toc" "page")))
   (TeX-run-style-hooks
    "inputenc"
    "latexsym"
    "amssymb"
    "amsmath"
    "svg"
    "subfig"
    "ku-frontpage"
    "float"
    "caption"
    "appendix"
    "fancyvrb"
    "url"
    "tabularx"
    "booktabs"
    "titlesec"
    "emoji"
    "semantic"
    "stmaryrd"
    "hyperref"
    "syntax"
    "listings"
    "xcolor")
   (TeX-add-symbols
    "phantomsection"
    "listofillustrations"
    "summaryname")
   (LaTeX-add-environments
    "changemargin"
    "Abstract"))
 :latex)

