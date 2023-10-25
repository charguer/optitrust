# DEPRECATED, COULD BE UPDATED
exit 1

TEMPLATE="variable_unfold"
TARGET="$1"
cp ${TEMPLATE}.ml ${TARGET}.ml
cp ${TEMPLATE}.cpp ${TARGET}.cpp
git add ${TARGET}.ml ${TARGET}.cpp
echo "code ${TARGET}.ml ${TARGET}.cpp"
