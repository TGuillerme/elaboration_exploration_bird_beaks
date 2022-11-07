## Remove spaces
for oldname in *.eml; do newname=`echo $oldname | sed -e 's/ //g'`; mv "$oldname" "$newname"; done;

## Loop through the mails
echo "" > report.txt
for f in *.eml
do
	## Job number
	JOB_ID=$(grep 'Subject: Job' $f | sed 's/.job) Complete//g' | sed 's/Subject: Job [0123456789][0123456789][0123456789][0123456789][0123456789][0123456789] (shapespace_allbirds_lvl_superorder_order_//g')
	## Job time
	WALL_TIME=$(grep 'Wallclock Time   =' $f | sed 's/ Wallclock Time   = //g')
	## Convert the job time in CPU.hours
	echo "${JOB_ID} - ${WALL_TIME}" >> report.txt
done

## Get the text ready for R
sed 's/ - /CPU.hours("/g' report.txt > tmp
mv tmp report.txt

## Manually search repalce \n0 by ") #0

## Then remove by search replace
> CPU\..*\n
\[[1]\] 

