#!/bin/bash

######################################################################
## - User's section

## - Define paths
srcdir=$PWD/src/
datadir=$PWD/input/
casename=rmp_TUTORIAL
nnode=2
mpiprocs=1
threads=1 

## - Source & target grids and remapping
SRC_GRID=atmt 
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
echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo "  Create rmp file nnodes=$nnode nprocs=$mpiprocs nthreads=$threads"
echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo '  Rundir  :' $rundir
echo '  Grids   : '$SRC_GRID'-->'$TGT_GRID
echo '  Remap   : '$remap
echo ' ' $exe1'  : '$nproc_exe1 'processes'
echo ' ' $exe2'  : '$nproc_exe2 'processes'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Copy everything needed into rundir
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm -fr $rundir
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc

ln -sf $srcdir/$exe1 $rundir/.
ln -sf $srcdir/$exe2 $rundir/.

cp -f $datadir/namcouple_${SRC_GRID}_${TGT_GRID} $rundir/namcouple

cat <<EOF >> $rundir/name_grids.dat
\$grid_source_characteristics
cl_grd_src='$SRC_GRID'
cl_remap='$remap'
\$end
\$grid_target_characteristics
cl_grd_tgt='$TGT_GRID'
\$end
EOF

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Creation of configuration scripts
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

if [[ ${machine} = '' ]]; then

  echo 'Please load environment.sh'
  stop

else

  echo '  You are using' ${machine}

fi

mpirun -np ${nproc_exe1} ./$exe1 : -np ${nproc_exe2} ./$exe2
EOF

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Execute the model 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if [ -e sbatch ]; then

  echo '  Submitting the job to queue using sbatch'
  sbatch $rundir/run_$casename

else

  cd $rundir
  . run_$casename
fi

echo '  Results are found in rundir : '$rundir 

