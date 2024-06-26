#!/bin/bash
#set -x

host=`uname -n`
user=`whoami`

## - Define paths
srcdir=`pwd`
datadir=$srcdir/input
casename=`basename $srcdir`

## - Define case
if [ $# -eq 0 ] ; then
   echo "Default usage: ./run_testinterp.sh 2_1_1 (i.e. nnodes=2, nprocs=1, nthreads=1)"
   echo "nnodes: total number of nodes fr the run"
   echo "nprocs: number of MPI tasks per node"
   echo "nthreads: number of OpenMP threads per MPI task"
   n_p_t=1
   nnode=2
   mpiprocs=1
   threads=1 
else
   n_p_t=$1
   nargs=`echo $n_p_t | awk -F _ '{print NF}'`
   if [ $nargs -ne 3 ] ; then
       echo "You can run this script without argument (default nnodes=2, nprocs=1, nthreads=1 will be used)"
       echo "or as ./run_testinterp.sh nnodes_ nprocs_nthreads where:"
       echo "nnodes: total number of nodes fr the run"
       echo "nprocs: number of MPI tasks per node"
       echo "nthreads: number of OpenMP threads per MPI task"
       exit
   else
       nnode=`echo $n_p_t | awk -F _ '{print $1}'`
       mpiprocs=`echo $n_p_t | awk -F _ '{print $2}'`
       threads=`echo $n_p_t | awk -F _ '{print $3}'`
   fi
fi

######################################################################
## - User's section
##
SRC_GRID=atmt
TGT_GRID=ocnt
remap=bilinear
rundir=$srcdir/${casename}_${SRC_GRID}_${TGT_GRID}_${remap}/rundir_${nnode}_${mpiprocs}_${threads}
##
## - End of user's section
######################################################################

typeset -Z4 nodes
nodes=$nnode
typeset -Z2 mpiprocesses
mpiprocesses=$mpiprocs
typeset -Z2 nthreads
nthreads=$threads

## - Name of the executables
exe1=model1
exe2=model2

## - Define number of processes to run each executable
(( nproc = $nnode * $mpiprocs ))
(( nproc_exe2 = $nproc / 2 ))
(( nproc_exe1 = $nproc - $nproc_exe2 ))

echo ''
echo '*****************************************************************'
echo '*** '$casename' : '$run
echo ''
echo "Running test_interpolation with nnodes=$nnode nprocs=$mpiprocs nthreads=$threads"
echo '*****************************************************************'
echo 'Source grid :' $SRC_GRID
echo 'Target grid :' $TGT_GRID
echo 'Rundir       :' $rundir
echo 'Architecture :' $arch
echo 'Host         : '$host
echo 'User         : '$user
echo 'Grids        : '$SRC_GRID'-->'$TGT_GRID
echo 'Remap        : '$remap
echo 'Threads      : '$threads
echo ''
echo $exe1' runs on '$nproc_exe1 'processes'
echo $exe2' runs on '$nproc_exe2 'processes'
echo ''
echo ''

## - Copy everything needed into rundir
\rm -fr $rundir/*
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc
#ln -sf $datadir/areas.nc  $rundir/areas.nc

ln -sf $srcdir/$exe1 $rundir/.
ln -sf $srcdir/$exe2 $rundir/.

cp -f $datadir/namcouple_${SRC_GRID}_${TGT_GRID} $rundir/namcouple

## - Grid source characteristics 
# If you add any additional lines in the namcouples given as examples you will have
# to change the 3 lines below 
SRC_GRID_TYPE=`sed -n 20p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # source grid type
SRC_GRID_PERIOD=`sed -n 17p $rundir/namcouple | tr -s ' ' | cut -d" " -f1` # "P" for periodic, "R" for non-periodic
SRC_GRID_OVERLAP=`sed -n 17p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # Number of overlapping grid points for periodic grids

echo "SRC_GRID_TYPE : $SRC_GRID_TYPE"
echo "SRC_GRID_PERIOD : $SRC_GRID_PERIOD"
echo "SRC_GRID_OVERLAP : $SRC_GRID_OVERLAP"

## - Create name_grids.dat from namcouple informations
cat <<EOF >> $rundir/name_grids.dat
&grid_source_characteristics
cl_grd_src='$SRC_GRID'
cl_remap='$remap'
cl_type_src='0'
cl_period_src='R'
il_overlap_src=$SRC_GRID_OVERLAP
/
&grid_target_characteristics
cl_grd_tgt='$TGT_GRID'
/
EOF
#
cd $rundir

######################################################################
### - Execute the model

echo 'Executing the model using mpirun'
mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2 > runjob.err

echo 'Results are found in rundir : '$rundir 

######################################################################

