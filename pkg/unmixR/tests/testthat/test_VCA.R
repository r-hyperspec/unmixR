source("testData.R")

test_that("VCA works on test data", {
    indices <- vcaFromScratch(testData, 2)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})