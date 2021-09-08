#!/bin/bash
##########################
# Script for initialising the minichains
##########################
#SYNTAX: sh make.minichains.jobs.sh -chain <chain_name> -n <replicates> -p <path (optional)>
#with:
#-chain <chain_name> the name of the chain
#-n <number_of_replicates> the number of replicates
#-p <path> optional, the path to the chain
##########################
#guillert(at)tcd.ie - 2021/09/08
##########################

#INPUT
## Input values
while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -c|--chain)
        CHAINNAME="$2"
        shift
        ;;
    -b|--replicates)
        REPLICATES="$2"
        shift
        ;;        
    -p|--path)
        PATH="$2"
        ;;
        *)
        ;;
esac
shift
done

## Creating the batch job template
cp job_template.job ${PATH}${CHAINNAME}_template.job
echo "" >> ${PATH}${CHAINNAME}_template.job
echo "## Run the script" >> ${PATH}${CHAINNAME}_template.job
echo "Rscript ${PATH}${CHAINNAME}_<REPLICATE>.script.R" >> ${PATH}${CHAINNAME}_template.job

## Creating the Rscript template
echo ".libPaths(\"~/homelib\")" > ${PATH}${CHAINNAME}_template.R
echo "library(MCMCglmm)" >> ${PATH}${CHAINNAME}_template.R
echo "library(mcmcmcglmmm)" >> ${PATH}${CHAINNAME}_template.R
echo "load(\"${PATH}${CHAINNAME}.rda\")" >> ${PATH}${CHAINNAME}_template.R
echo "model <- run.mini.chains(${CHAINNAME}, 1)" >> ${PATH}${CHAINNAME}_template.R
echo "save(model, file = \"${PATH}${CHAINNAME}_<REPLICATE>.rda\")" >> ${PATH}${CHAINNAME}_template.R

## Looping through each templates to create the replicates
for (( i = 1; i <= ${REPLICATES}; i++ ));
do
    sed 's/<REPLICATE>/'"${i}"'/g' ${PATH}${CHAINNAME}_template.job > ${PATH}${CHAINNAME}_${i}.job
    sed 's/<REPLICATE>/'"${i}"'/g' ${PATH}${CHAINNAME}_template.R > ${PATH}${CHAINNAME}_${i}.R
done

## Cleaning
rm tmp
rm ${PATH}${CHAINNAME}_template.R
rm ${PATH}${CHAINNAME}_template.job

## Prompt
echo "## To qsub all chains, run the following:"
echo "for file in ${PATH}${CHAINNAME}_*.job ; do echo ${file} ; qsub ${file}; done"