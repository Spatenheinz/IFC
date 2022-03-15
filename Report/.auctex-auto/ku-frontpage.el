(TeX-add-style-hook
 "ku-frontpage"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "danish" "english")))
   (TeX-run-style-hooks
    "eso-pic"
    "graphicx"
    "fix-cm"
    "ae"
    "aecompl"
    "ifthen"
    "babel")
   (TeX-add-symbols
    '("dato" 1)
    '("vejleder" 1)
    '("undertitel" 1)
    '("titel" 1)
    '("forfatter" 1)
    '("opgave" 1)
    "SPROG"
    "AFDELING"
    "FARVE"
    "FORSIDE"
    "OPGAVE"
    "FORFATTER"
    "TITEL"
    "UNDERTITEL"
    "VEJLEDER"
    "AFLEVERINGSDATO"
    "tyk"
    "tynd"
    "maketitle"))
 :latex)

