testData <- expand.grid ( 0 : 3, 0 : 3)
testData [, 3] <- 3 - rowSums (testData)
testData <- as.matrix (testData [testData [, 3] >= 0,])
dimnames(testData) <- list(samples = 1:10, wavelengths = paste("L", 1:3, sep = ""))

correct <- c(1, 4, 10)