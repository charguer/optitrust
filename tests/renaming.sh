
for tr in $(ls -1 combi/*.ml basic/*.ml); do
  base="`basename ${tr} .ml`"
  categ="${base%_*}"
  folder="${categ}/${base}"
  echo "mkdir ${folder}"
  echo "mv ${base}.ml ${base}.cpp ${base}_exp.cpp ${folder}"
done



exit


cd basic
rm -f *_with_lines.ml
ls -1 *.ml > list_basic.ml
cd ..


cd combi
rm -f *_with_lines.ml
ls -1 *.ml > list_combi.ml
cd ..

grep -Fxf list_basic.txt list_combi.txt > list_common.ml

for tr in $(cat list_common.ml); do
  base="`basename ${tr} .ml`"
  echo "mv ${tr} ${base}_basic.ml"
  echo "mv ${base}.cpp ${base}_basic.cpp"
  echo "mv ${base}_exp.cpp ${base}_basic_exp.cpp"
done

