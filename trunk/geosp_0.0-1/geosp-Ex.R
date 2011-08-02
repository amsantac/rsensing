pkgname <- "geosp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('geosp')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("fbr")
### * fbr

flush(stderr()); flush(stdout())

### Name: fbr
### Title: titulo fbrRd
### Aliases: fbr
### Keywords: spatial

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## Ejemplos
4*2



cleanEx()
nameEx("geosp_0.0-1-package")
### * geosp_0.0-1-package

flush(stderr()); flush(stdout())

### Name: geosp-package
### Title: Rd esto hace
### Aliases: geosp geosp
### Keywords: package spatial

### ** Examples

8*4



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
