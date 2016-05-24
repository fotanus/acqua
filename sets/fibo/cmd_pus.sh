echo -n "["
for i in *pus;
do
  cat $i | grep -- ---- | wc -l;
done | tr '\n' ','
echo "]"
