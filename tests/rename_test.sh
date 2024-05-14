# TODO generalize and document the script

exit

$1 should be $FOLDER
$2 should be $NEWNAME

FOLDER="tests/loop/foo"
NEWNAME="bar"

# should be computed
PARENT_FOLDER="tests/loop"
OLDNAME="foo"


cd ${PARENT_FOLDER}
git mv ${OLDNAME} ${NEWNAME}
git mv
for i in ${OLDNAME}*; do
  git mv $i ${i/${OLDNAME}/${NEWNAME}} || echo "ignoring file, you may call: rm ${OLDNAME}*"; ^X
done  

