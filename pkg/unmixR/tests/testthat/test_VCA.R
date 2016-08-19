library ("unmixR")
context ("VCA")

testData <- unmixR:::.testdata$x
correct  <- unmixR:::.correct

# BH: Expanded error messages
# BH: Which ever one of these is related to vca05, it
# may fail with p = 2 (see note in vca05.R)

test_that("vca05 works on test data, p = 2", {
    indices <- vca(testData, 2, "05")$indices
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vca05 works on test data, p = 3", {
    indices <- vca(testData, 3, "05")$indices
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vcaLopez2012 works on test data, p = 2", {
    indices <- vca(testData, 2, "Lopez2012")$indices
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})

test_that("vcaLopez2012 works on test data, p = 3", {
    indices <- vca(testData, 3, "Lopez2012")$indices
    expect_true(all(indices %in% correct))
    expect_false(any(duplicated(indices)))
})
