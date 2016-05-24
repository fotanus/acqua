echo -n "[" ; for i in *out; do cat $i | grep -- ---- | wc -l; done | tr '\n' ','; echo "]" -n
