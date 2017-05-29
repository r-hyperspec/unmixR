

# Run spell checking on the .Rnw before submission

library("hunspell")

txt <- readLines("unmixR.Rnw")
res <- hunspell(txt, "latex")
bad <- sort(unique(unlist(res)))
