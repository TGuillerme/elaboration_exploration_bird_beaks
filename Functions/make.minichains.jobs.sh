## Shell part

#!/bin/bash
#
#$ -l h_rt=00:01:00
# below -m option can have any combination of b , e or a  to imply when to to send email where;
#    b = begining of job  e = end of job  a = in case job gets aborted unexpectedly 
#$ -m ae
#$ -M guillert@tcd.ie

## Load R
module load apps/R/4.0.3/gcc-8.2.0

## Run the script
Rscript ${CHAINNAME}_${REPLICATE}.script.R







## R part
library(MCMCglmm)
library(beer)
load(${CHAINNAME})
model <- run.mini.chains(${CHAINNAME}, 1)
save(model, file = "${CHAINNAME}_${REPLICATE}.rda")
