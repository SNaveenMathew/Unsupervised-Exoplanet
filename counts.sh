rm counts.txt
touch counts.txt
for i in `ls -1 data/*.tbl`
do
	head -n 12 $i | awk -F" " '{if(NR==9){ x=$NF; } if(NR==12){ y=$NF; print x"\t"y; } }' >> counts.txt
done
