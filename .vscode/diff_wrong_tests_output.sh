WRONG_TESTS=$(cat wrong.tests)

for test in $WRONG_TESTS
do
  code -d ${test%.ml}_out.cpp ${test%.ml}_exp.cpp
done
