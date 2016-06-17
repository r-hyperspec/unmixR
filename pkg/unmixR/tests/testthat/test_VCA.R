library ("unmixR")

testData <- unmixR:::.testdata$x
correct  <- unmixR:::.correct

# BH: Expanded error messages
# BH: Which ever one of these is related to vca05, it
# may fail with p = 2 (see note in vca05.R)

test_that("vcaFromScratch works on test data, p = 2", {
    indices <- vcaFromScratch(testData, 2)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vcaLopezFromScratch works on test data, p = 2", {
    indices <- vcaLopezFromScratch(testData, 2)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vcaFromScratch works on test data, p = 3", {
    indices <- vcaFromScratch(testData, 3)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vcaLopezFromScratch works on test data, p = 3", {
    indices <- vcaLopezFromScratch(testData, 3)
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})
