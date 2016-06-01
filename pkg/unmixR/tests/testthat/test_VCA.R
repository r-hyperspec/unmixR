source("testData.R")

test_that("VCA works on test data", {
    indices <- vcaFromScratch(testData, 2)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("VCA works on test data", {
    indices <- vcaLopezFromScratch(testData, 2)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("VCA works on test data", {
    indices <- vcaFromScratch(testData, 3)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("VCA works on test data", {
    indices <- vcaLopezFromScratch(testData, 3)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})
