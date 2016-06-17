# get all tests attached to objects
tests <- eapply(env=getNamespace ("unmixR"), FUN=get.test, all.names=TRUE)
tests <- tests [! sapply (tests, is.null)]

for (t in seq_along (tests))
  tests [[t]] ()

