DIR=$1
FILE=$2
LINE=$3

# Launch the directory containing the file
cd ${DIR}

# Extract the target line, then extract the basename from it
TEST_ML=`sed -n ${LINE}p ${FILE} | grep -Po '[^\s]*\.ml'`

# Get the name of the .cpp file associated with the .ml file
TEST_CPP="`basename ${TEST_ML} .ml`.cpp"

# Open the two files in VScode
code ${TEST_ML} ${TEST_CPP}
