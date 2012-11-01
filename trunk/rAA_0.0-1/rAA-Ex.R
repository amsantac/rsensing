pkgname <- "rAA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('rAA')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AQstats")
### * AQstats

flush(stderr()); flush(stdout())

### Name: AQstats
### Title: Generate a SpatialPoints object corresponding to the best result
###   obtained in an optimized network
### Aliases: AQstats
### Keywords: spatial

### ** Examples

data(simData)
resAQ <- AQstats(simData)
resAQ



cleanEx()
nameEx("simData")
### * simData

flush(stderr()); flush(stdout())

### Name: simData
### Title: Map of total soil carbon stock (t/ha) at 0-10 cm depth
### Aliases: simData
### Keywords: datasets

### ** Examples

data(simData)
resAQ <- AQstats(simData)
resAQ



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
