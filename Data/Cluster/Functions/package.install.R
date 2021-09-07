.libPaths("~/homelib")
.libPaths()
install.packages(c("cubature", 
                   "tensorA", 
                   "corpcor", 
                   "ape", 
                   "Matrix", 
                   "coda",
                   "MCMCglmm"),
                 repos = "https://cloud.r-project.org",
                 lib = "~/homelib")
library(MCMCglmm, lib.loc = "~/homelib")
data(PlodiaPO)  
model1<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
 nitt=1300, burnin=300, thin=1)
save(model1, file = "model1.rda")
