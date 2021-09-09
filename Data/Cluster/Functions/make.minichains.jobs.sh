##########################
# Script for initialising the minichains
##########################
#SYNTAX: sh make.minichains.jobs.sh -c <chain_name> -r <replicates> -p <path (optional)>
#with:
#-c <chain_name> the name of the chain
#-r <number_of_replicates> the number of replicates
#-p <path> optional, the path to the chain
##########################
#guillert(at)tcd.ie - 2021/09/09
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
## Bug on this line!!!!


## Creating the Rscript template
echo ".libPaths(\"~/homelib\")" > ${LOCALPATH}${CHAINNAME}_template.R
echo "library(MCMCglmm)" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "library(mcmcmcglmmm)" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "load(\"${LOCALPATH}${CHAINNAME}.rda\")" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "model <- run.mini.chains(${CHAINNAME}, replicates = 1, record.tree = TRUE)" >> ${LOCALPATH}${CHAINNAME}_template.R
echo "save(model, file = \"${LOCALPATH}${CHAINNAME}_<REPLICATE>.rda\")" >> ${LOCALPATH}${CHAINNAME}_template.R

## Looping through each templates to create the replicates
for (( i = 1; i <= ${REPLICATES}; i++ ));
do
    sed 's/<REPLICATE>/'"${i}"'/g' ${LOCALPATH}${CHAINNAME}_template.job > ${LOCALPATH}${CHAINNAME}_${i}.job
    sed 's/<REPLICATE>/'"${i}"'/g' ${LOCALPATH}${CHAINNAME}_template.R > ${LOCALPATH}${CHAINNAME}_${i}.R
done

## Cleaning
rm ${LOCALPATH}${CHAINNAME}_template.R
rm ${LOCALPATH}${CHAINNAME}_template.job

## Prompt
echo "## To qsub all chains, run the following:"
echo "for file in ${LOCALPATH}${CHAINNAME}_*.job ; do echo \${file} ; qsub \${file}; done"