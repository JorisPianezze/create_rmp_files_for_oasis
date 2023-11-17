#!/bin/ksh
#set -x

host=`uname -n`
user=`whoami`

## - Define paths
srcdir=`pwd`
datadir=$srcdir/data_oasis3_mnh_ww3_croco_mapio
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

## - Source & target grids and remapping (corresponding to files and namcouple in data_oasis3)
SRC_GRID=wavt
TGT_GRID=atmt
remap=bilinear

rundir=$srcdir/${casename}_${SRC_GRID}_${TGT_GRID}_${remap}_mapio/rundir_${nnode}_${mpiprocs}_${threads}

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
\rm -fr $rundir
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc
ln -sf $datadir/areas.nc  $rundir/areas.nc

ln -sf $srcdir/$exe1 $rundir/.
ln -sf $srcdir/$exe2 $rundir/.

cp -f $datadir/namcouple_${SRC_GRID}_${TGT_GRID} $rundir/namcouple

## - Grid source characteristics and create name_grids.dat
SRC_GRID_TYPE=`sed -n 26p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # source grid type
SRC_GRID_PERIOD=`sed -n 23p $rundir/namcouple | tr -s ' ' | cut -d" " -f1` # "P" for periodic, "R" for non-periodic
SRC_GRID_OVERLAP=`sed -n 23p $rundir/namcouple | tr -s ' ' | cut -d" " -f2` # Number of overlapping grid points for periodic grids

cat <<EOF >> $rundir/name_grids.dat
\$grid_source_characteristics
cl_grd_src='$SRC_GRID'
cl_remap='$remap'
cl_type_src='$SRC_GRID_TYPE'
cl_period_src='$SRC_GRID_PERIOD'
il_overlap_src=$SRC_GRID_OVERLAP
\$end
\$grid_target_characteristics
cl_grd_tgt='$TGT_GRID'
\$end
EOF
#
cd $rundir

######################################################################
## - Creation of configuration scripts

###---------------------------------------------------------------------
### BEAUFIX
###---------------------------------------------------------------------
ncore_per_node=40
(( cpus_per_task = $ncore_per_node * 2 / $mpiprocs ))
timreq=24:00:00
cat <<EOF > $rundir/run_$casename
#!/bin/bash
#SBATCH --exclusive
#SBATCH --partition=normal256
#SBATCH --job-name ${remap}_${nthreads}
# Time limit for the job
#SBATCH --time=$timreq
#SBATCH -o $rundir/$casename.o
#SBATCH -e $rundir/$casename.e
# Number of nodes
#SBATCH --nodes=$nnode
# Number of MPI tasks per node
#SBATCH --ntasks-per-node=$mpiprocs
# Number of threads per MPI task ombre de thread openmp par proc MPI = nombre de coeur par proc
#SBATCH -c $cpus_per_task
ulimit -s unlimited
# rundir must be in the TMPDIR
cd \$TMPDIR
cp $rundir/* \$TMPDIR
#
module load intel/2019.5.281
module load openmpi/intel/4.0.2.2
export NETCDF_CONFIG=${HOME}/MNH/MNH-V5-4-4/src/LIB/netcdf-LXifort-R8I4/bin/nf-config
export OASISDIR=${HOME}/OASIS/oasis3-mct_4.0/BELENOS_oa3-mct
#
time mpirun -np ${nproc_exe1} ./$exe1 : -np ${nproc_exe2} ./$exe2
#
cp * $rundir 

EOF

######################################################################
### - Execute the model

echo 'Submitting the job to queue using sbatch'
sbatch $rundir/run_$casename
squeue -u $user

echo $casename 'is executed or submitted to queue.'
echo 'Results are found in rundir : '$rundir 

######################################################################

