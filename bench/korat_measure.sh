#!/bin/bash

SIZE=15
REPEAT=3
TIMEOUT=210s

PROJECT_DIR="./"
KORAT_BUILD="$PROJECT_DIR/../korat-enum/"
OUTPUTDIR="$PROJECT_DIR/tmp/korat"
LOG_FILE="$OUTPUTDIR/korat.log"
SLEEPTIME=1

EXAMPLES=(
#korat.examples.doublylinkedlist.DoublyLinkedList, "Binary Search Tree",
korat.examples.searchtree.SearchTree "BinarySearchTree" ""
korat.examples.sortedlist.SortedList "SortedList" ""
korat.examples.redblacktree.RedBlackTree "Red-BlackTree" ""
korat.examples.heaparray.HeapArray "HeapArray" ""
korat.examples.dagb.DAG "DirectedAcyclicGraph" ""
korat.examples.classinterfacedag.DAG "Class-InterfaceDAG" ",2"
)
EXAMPLES_NUM=17

KORAT_RUN="ant -f $KORAT_BUILD -logger org.apache.tools.ant.listener.ProfileLogger run -DkoratArgs='"
KORAT_RUN_END="'"
KORAT_ARGS_CLASS_PART="--class"
KORAT_ARGS_ARGS_PART="--args"

rm -f $SAVEFILE
rm -f $LOG_FILE

# ------------------ usage function --------------------------------
usage()
{
cat << EOF
usage: $0 options

This script compiles, runs, and measures korat.

OPTIONS:
   -h      Show this message
   -s:x    Size given to Korat (x)
EOF
}

#get parameters
while getopts "s:o:e:" opt; do
  case $opt in
    s)
      SIZE="${OPTARG}"
      echo "Size set to: $SIZE" >&2
      ;;
    o)
      OUTPUTDIR=$OPTARG
      echo "Output dir set to: $OUTPUTDIR" >&2
      ;;
    e)
      EXAMPLES=( "$OPTARG" )
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      usage
      exit 1
      ;;
    h)
      usage
      exit 1
      ;;
  esac
done

if [ -d "${OUTPUTDIR}" ]; then
    echo "Directory ${OUTPUTDIR} already here (will try to delete)"
    rm -rfI ${OUTPUTDIR}
    #exit -1
fi

mkdir -p ${OUTPUTDIR}
LOG_FILE="${OUTPUTDIR}/run.log"

echo "Build korat"
#ant clean
#ant build

echo "Measurements:"

SIZES=`seq 1 $SIZE`

for EXAMPLE_IND in `seq 0 3 $EXAMPLES_NUM`;
do
  EXAMPLE="${EXAMPLES[$EXAMPLE_IND]}"
  let EXAMPLE_IND_1=EXAMPLE_IND+1
  EXAMPLE_NAME="${EXAMPLES[$EXAMPLE_IND_1]}"
  let EXAMPLE_IND_2=EXAMPLE_IND_1+1
  EXTRAARG="${EXAMPLES[$EXAMPLE_IND_2]}"
  
  echo -e "Example $EXAMPLE, example name $EXAMPLE_NAME, extra args $EXTRAARG"

  #mkdir -p ${OUTPUTDIR}/${EXAMPLE##*.}
  #SAVEFILE="$OUTPUTDIR/${EXAMPLE##*.}/Summary.txt"
  SAVEFILE="$OUTPUTDIR/${EXAMPLE_NAME}"
  echo "Save file is $SAVEFILE"
  #echo -e "Size\tAverage time" >> $SAVEFILE
    
  for SIZE in $SIZES; do
    ARG="$SIZE$EXTRAARG"
    echo "Measuring $EXAMPLE for $ARG"

    RUN_COMMAND="timeout $TIMEOUT $KORAT_RUN $KORAT_ARGS_CLASS_PART $EXAMPLE $KORAT_ARGS_ARGS_PART $ARG $KORAT_RUN_END"
    echo "Running with command: $RUN_COMMAND"  
    
    #LOG_FILE="$OUTPUTDIR/${EXAMPLE##*.}/$SIZE.txt"
    #SUM=0
    printf "%d\t" $SIZE >> $SAVEFILE
    for (( i=1; i<=$REPEAT; i++ ))
    do
	#GREPD=`"$RUN_COMMAND" 2> /dev/null | tee -a $LOG_FILE | grep "Overall time:"`
	GREPD=`eval $RUN_COMMAND | tee -a $LOG_FILE | grep "Overall time:"`
	#echo $COMMAND_OUT
	#exit
	
	RET_CODE=$?
	if [ $RET_CODE == 1 ]; then
	  printf "This measurement timed out"
	  #SUM=-1
	  break
	else	  
	  # get time without copying
	  ADD=`printf "%s\n" "$GREPD" |  grep "Overall time:" | sed 's/\[java\] Overall time: \([0-9\.]\+\) s./\1/'`	
	  echo "Run #$i, running time: $ADD"
	  #SUM=`echo "$SUM + $ADD" | bc -l`
	  printf "%.5f\t" $ADD >> $SAVEFILE
	fi
		    
	sleep $SLEEPTIME
    done	
    #AVG=`echo "$SUM / $REPEAT" | bc -l`
    #echo "Average accross $REPEAT runs for size $SIZE is $AVG"    
    #printf "%d\t%.5f\n" $SIZE $AVG >> $SAVEFILE
    printf "\n" >> $SAVEFILE
  done
  
  #echo "Log files can be found in $OUTPUTDIR/${EXAMPLE##*.}"
done

exit