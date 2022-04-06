(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt" "t")))
   (TeX-run-style-hooks
    "latex2e"
    "intro"
    "interpreter"
    "div2"
    "l"
    "undefined"
    "conclusion"
    "beamer"
    "beamer10"
    "pslatex"
    "fontspec"
    "caption"
    "subcaption"
    ""
    "amssymb"
    "listings"
    "xcolor"
    "coloremoji"))
 :latex)

