#!/bin/sh
##########################
# Script for initialising the minichains
##########################
#SYNTAX: sh make.minichains.jobs.sh -c <chain_name> -r <replicates> -p <path (optional)>
#with:
#-c <chain_name> the name of the chain
#-r <number_of_replicates> the number of replicates
#-p <path> optional, the path to the chain
##########################
#guillert(at)tcd.ie - 2021/10/05
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
    -r|--replicates)
        REPLICATES="$2"
        shift
        ;;        
    -p|--path)
        LOCALPATH="$2"
        ;;
        *)
        ;;
esac
shift
done

## Creating the batch job template
cp job_template.job ${LOCALPATH}${CHAINNAME}_template.job
echo "" >> ${LOCALPATH}${CHAINNAME}_template.job
echo "## Run the script" >> ${LOCALPATH}${CHAINNAME}_template.job
echo "Rscript ${LOCALPATH}${CHAINNAME}_<REPLICATE>.R" >> ${LOCALPATH}${CHAINNAME}_template.job

## Creating the Rscript template
echo ".libPaths(\"~/homelib\")" > ${LOCALPATH}${CHAINNAME}_template.R
echo "library(MCMCglmm)" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "library(mcmcmcglmmm)" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "load(\"${LOCALPATH}${CHAINNAME}.mini.chains\")" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "model <- run.mini.chains(${CHAINNAME}, replicates = 2, record.tree = TRUE)" >> ${LOCALPATH}${CHAINNAME}_template.R
# echo "save(model, file = \"${LOCALPATH}${CHAINNAME}_<REPLICATE>.rda\")" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "save(model, file = \"/data/bo1tgu/shapespace_passerines_tmp_store/${CHAINNAME}_<REPLICATE>.rda\")" >> ${LOCALPATH}${CHAINNAME}_template.R


## Get the number of digits in REPLICATES
digits=${#REPLICATES}
if [ "$digits" == 1 ]
then 
    zeros="000"
    start=""
else
    if [ "$digits" == 2 ]
    then
        zeros="00"
        start="0"
    else
        if [ "$digits" == 1 ]
        then
            zeros="0"
            start="00"
        else
            zeros=""
            start="000"
        fi
    fi
fi

## Looping through each templates to create the replicates
for i in $(seq -w ${start}1 ${REPLICATES})
do
    n=${zeros}${i}
    sed 's/<REPLICATE>/'"${n}"'/g' ${LOCALPATH}${CHAINNAME}_template.job > ${LOCALPATH}${CHAINNAME}_${n}.job
    sed 's/<REPLICATE>/'"${n}"'/g' ${LOCALPATH}${CHAINNAME}_template.R > ${LOCALPATH}${CHAINNAME}_${n}.R
done

## Cleaning
rm ${LOCALPATH}${CHAINNAME}_template.R
rm ${LOCALPATH}${CHAINNAME}_template.job

## Prompt
echo "## To qsub all chains, run the following:"
echo "for file in ${LOCALPATH}${CHAINNAME}_*.job ; do echo \${file} ; qsub \${file}; done"