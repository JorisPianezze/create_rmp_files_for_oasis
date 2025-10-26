#!/bin/ksh
#set -x

######################################################################
## - User's section

## - Define paths
srcdir=/home/cnrm_other/ge/erla/pianezzej/SAVE/scratch/config_NANMADOL/4_input_OASIS/create_rmp_files_for_oasis/src/
datadir=/home/cnrm_other/ge/erla/pianezzej/SAVE/scratch/config_NANMADOL/4_input_OASIS/create_rmp_files_for_oasis/input/
casename=rmp_NANMADOL
n_p_t=1
nnode=2
mpiprocs=1
threads=1 

## - Source & target grids and remapping (corresponding to files and namcouple in data_oasis3)
SRC_GRID=wavt 
TGT_GRID=ocnt
remap=bilinear

rundir=`pwd`/${casename}_${SRC_GRID}_${TGT_GRID}_${remap}/rundir_${nnode}_${mpiprocs}_${threads}

## - Name of the executables
exe1=model1
exe2=model2

nproc_exe1=1
nproc_exe2=1

## - End of user's section
#####################################################################

echo ''
echo '*****************************************************************'
echo '*** '$casename' : '$run
echo ''
echo "Running test_interpolation with nnodes=$nnode nprocs=$mpiprocs nthreads=$threads"
echo '*****************************************************************'
echo 'Source grid :' $SRC_GRID
echo 'Target grid :' $TGT_GRID
echo 'Rundir       :' $rundir
echo 'Grids        : '$SRC_GRID'-->'$TGT_GRID
echo 'Remap        : '$remap
echo 'Threads      : '$threads
echo ''
echo $exe1' runs on '$nproc_exe1 'processes'
echo $exe2' runs on '$nproc_exe2 'processes'
echo ''
echo ''

## - Copy everything needed into rundir
rm -fr $rundir
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc

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

cat <<EOF > $rundir/run_$casename
#!/bin/bash
#SBATCH --exclusive
#SBATCH --partition=normal256
#SBATCH --job-name remap
#SBATCH --time=1-00:00:00
#SBATCH -o output.o
#SBATCH -e error.e
#SBATCH -N 2
#SBATCH -n 2

ulimit -s unlimited

source /home/cnrm_other/ge/erla/pianezzej/models_NANMADOL/environment.sh

time mpirun -np ${nproc_exe1} ./$exe1 : -np ${nproc_exe2} ./$exe2

EOF

######################################################################
### - Execute the model

echo 'Submitting the job to queue using sbatch'
sbatch $rundir/run_$casename

echo $casename 'is executed or submitted to queue.'
echo 'Results are found in rundir : '$rundir 

######################################################################

