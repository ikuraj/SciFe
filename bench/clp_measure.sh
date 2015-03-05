#!/bin/bash

CLP_DIR='../../research/12-TAP'
PROJECT_DIR="./"
OUTPUTDIR="$PROJECT_DIR/tmp/clp"
LOG_FILE="$OUTPUTDIR/clp.log"

RUN_PREFIX="timeout 210s"

mkdir -p $OUTPUTDIR

# large stack for GNU Prolog
GLOBALSZ=131072; export GLOBALSZ

VLIB="$CLP_DIR/measure.pl"

## SIZE BOUNDS for each program ---------------------------

# RBTREEs (Fig.3)
SB2G=15
SBsG=20
# DISJOINTSETs (Fig.4)
SBDG=15
# HEAPARRAYs (Fig.4)
SBHG=15
# SEARCHTREEs (Fig.4)
SBTG=15
# SORTEDLISTs (Fig.4)
SBLG=16

echo "## Experiments started (MEASURING TIME) ##"

##### RBTREES #####

gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/rbtrees/rbtrees_2_gp.pl -o $CLP_DIR/rbtrees/rbtrees_2_gp
gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/rbtrees/rbtrees_sync_gp.pl -o $CLP_DIR/rbtrees/rbtrees_sync_gp

# rbtree 2
rm -f $OUTPUTDIR/RedBlackTree
touch $OUTPUTDIR/RedBlackTree
for (( i=0; i<=$SB2G; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/rbtrees/rbtrees_2_gp rbtree $i >>  $OUTPUTDIR/RedBlackTree

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- RBTrees 2 done."

# rbtree sync
rm -f $OUTPUTDIR/RedBlackTree_sync
touch $OUTPUTDIR/RedBlackTree_sync
for (( i=0; i<=$SBsG; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/rbtrees/rbtrees_sync_gp rbtree $i >>  $OUTPUTDIR/RedBlackTree_sync

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- RBTrees sync done."

##### DISJOINTSETs #####

gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/disjset/disjset_gp.pl -o $CLP_DIR/disjset/disjset_gp

rm -f $OUTPUTDIR/DisjointSet
touch $OUTPUTDIR/DisjointSet
for (( i=0; i<=$SBDG; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/disjset/disjset_gp disjset $i >> $OUTPUTDIR/DisjointSet

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- DisjointSets done."

##### HEAPARRAYs #####

gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/heaparray/heaparray_gp.pl -o $CLP_DIR/heaparray/heaparray_gp

rm -f $OUTPUTDIR/HeapArray
touch $OUTPUTDIR/HeapArray
for (( i=0; i<=$SBHG; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/heaparray/heaparray_gp heaparray $i >> $OUTPUTDIR/HeapArray

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- HeapArrays done."

##### SEARCHTREEs #####

gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/searchtree/searchtree_gp.pl -o $CLP_DIR/searchtree/searchtree_gp

rm -f $OUTPUTDIR/BinarySearchTree
touch $OUTPUTDIR/BinarySearchTree
for (( i=0; i<=$SBTG; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/searchtree/searchtree_gp searchtree $i >> $OUTPUTDIR/BinarySearchTree

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- SearchTrees done."

##### SORTEDLISTs #####

gplc $VLIB $CLP_DIR/time_gp.pl $CLP_DIR/sortedlist/sortedlist_gp.pl -o $CLP_DIR/sortedlist/sortedlist_gp

rm -f $OUTPUTDIR/SortedList
touch $OUTPUTDIR/SortedList
for (( i=0; i<=$SBLG; i++ ))
do
  echo -n "."
  $RUN_PREFIX $CLP_DIR/sortedlist/sortedlist_gp sortedlist $i >> $OUTPUTDIR/SortedList

  if [ $? == 1 ] ; then
    echo 'Skipping other sizes because of timeout'
    break
  fi
done

echo -e "
- SortedLists done."
