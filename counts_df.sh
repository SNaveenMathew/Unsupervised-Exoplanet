awk -F"\t" '{a[$1]=$2;} END { for(x in a) { print x"\t"a[x]} }' counts.txt
