##########################
# Script for renaming the hundreds on mini chains outputs
##########################
#SYNTAX: sh rename.output100.sh -c <chain_name> -h <hundreds>
#with:
#-c <chain_name> the name of the chain
#-h <hundreds> the number of hundreds
##########################
#guillert(at)tcd.ie - 2021/11/12
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
    -h|--hundreds)
        HUNDREDS="$2"
        ;;
        *)
        ;;
esac
shift
done

for FILE in ${CHAINNAME}_00*.rda
do
    NEWNAME=$(echo ${FILE} | sed 's/_00/_0'"${HUNDREDS}"'/')
    mv ${FILE} ${NEWNAME}
done
