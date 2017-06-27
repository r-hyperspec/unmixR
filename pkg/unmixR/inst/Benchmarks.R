
### Script to run unmixR benchmarks

# Do not run this without looking it over!
# Should be run in batch, with Cuprite350 in a separate run

library("unmixR")
library("microbenchmark")
library("R.utils")
library("ggplot2")

### laser data set

res1 <- microbenchmark(
	Brute = nfindr(laser, p = 2, method = "Brute"),
	Winter = nfindr(laser, p = 2, method = "99"),
	LDU = nfindr(laser, p = 2, method = "LDU"),
	SeqLDU = nfindr(laser, p = 2, method = "SeqLDU"),
	vca05 = vca(laser, p = 2, method = "05"),
	vcaLopez2012 = vca(laser, p = 2, method = "Lopez2012"),
	ICE = ice(laser, p = 2),
	times = 100L)
	
saveObject(res1, file = "laser_benchmark.Rdata")

p1 <- autoplot(res1)
p1 <- p1 + ggtitle("laser data set, p = 2, 100 replicates")

pdf("laser_Benchmark.pdf", width = 7, height = 5)
print(p1)
dev.off()

### chondro data set

res2 <- microbenchmark(
	Winter = nfindr(chondro, p = 4, method = "99"),
	LDU = nfindr(chondro, p = 4, method = "LDU"),
	SeqLDU = nfindr(chondro, p = 4, method = "SeqLDU"),
	vca05 = vca(chondro, p = 4, method = "05"),
	vcaLopez2012 = vca(chondro, p = 4, method = "Lopez2012"),
	ICE = ice(chondro, p = 4),
	times = 100L)

saveObject(res2, file = "chondro_benchmark.Rdata")

p2 <- autoplot(res2)
p2 <- p2 + ggtitle("chondro data set, p = 4, 100 replicates")

pdf("chondro_Benchmark.pdf", width = 7, height = 5)
print(p2)
dev.off()

### Cuprite 350 data set (there is a copy in the vignette folder)

Cup350 <- loadObject("Cuprite350x350.Rdata")

# These settings require ~ 20 hr CPU time
reps <- 20
p <- 20

res3 <- microbenchmark(
	Winter = nfindr(Cup350, p = p, method = "99"),
	LDU = nfindr(Cup350, p = p, method = "LDU"),
	SeqLDU = nfindr(Cup350, p = p, method = "SeqLDU"),
	vca05 = vca(Cup350, p = p, method = "05"),
	vcaLopez2012 = vca(Cup350, p = p, method = "Lopez2012"),
	times = reps)

saveObject(res3, file = "Cup350_benchmark.Rdata")

p3 <- autoplot(res3)
p3 <- p3 + ggtitle("cuprite 350 data set, p = 20, 20 replicates")

pdf("Cup350_Benchmark.pdf", width = 7, height = 5)
print(p3)
dev.off()
