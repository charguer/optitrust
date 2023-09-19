FILE=$1

BASE=${FILE%.*}
ARGS="${BASE}.ml ${BASE}.cpp"

if [ -f ${BASE}.ml ]
then
  code ${ARGS}
else
  echo "open_test.sh: ${FILE} is not a cpp or a ml file"
  exit 1
fi
