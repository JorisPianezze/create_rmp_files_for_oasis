#!/bin/bash

if [ "$#" -lt 2 ]; then
   echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   echo "                                              "
   echo "                 E R R O R                    "
   echo "                                              "
   echo "            arguments are missing !           "
   echo "                                              "
   echo "  ./create_rmp_files.sh <src_grid> <tgt_grid> "
   echo "                                              "
   echo "  with:                                       "
   echo "  <src_grid> = atmt, wavt or ocnt             "
   echo "  <tgt_grid> = atmt, wavt or ocnt             "
   echo "                                              "
   echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
   exit 1
fi

if [[ "$1" != "atmt" && "$1" != "wavt" && "$1" != "ocnt" ]]; then
    echo "Error: Your first argument has to be atmt, wavt or ocnt."
    exit 1
fi

if [[ "$2" != "atmt" && "$2" != "wavt" && "$2" != "ocnt" ]]; then
    echo "Error: Your second argument has to be atmt, wavt or ocnt."
    exit 1
fi

######################################################################
## - User's section

## - Define case name
casename=rmp_TUTORIAL

## - Define paths
srcdir=$PWD/src/
datadir=$PWD/input/

## - Source & target grids and remapping method
src_grid=$1
tgt_grid=$2
remap=bilinear

## - Number of proc for each exe
nproc_model1=1
nproc_model2=1

## - Node reservation in case of using sbatch
nnode=2
mpiprocs=1
threads=1

## - End of user's section
#####################################################################

rundir=`pwd`/${casename}_${src_grid}_${tgt_grid}_${remap}/rundir_${nnode}_${mpiprocs}_${threads}

echo ''
echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo '  Create rmp file with following characteristicsi :              '
echo ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo '  Rundir  :' $rundir
echo '  Grids   : '$src_grid'-->'$tgt_grid
echo '  Remap   : '$remap
echo '  Model 1 : '$nproc_model1 'processes'
echo '  Model 2 : '$nproc_model2 'processes'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Copy everything needed into rundir
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm -fr $rundir
mkdir -p $rundir

ln -sf $datadir/grids.nc  $rundir/grids.nc
ln -sf $datadir/masks.nc  $rundir/masks.nc

ln -sf $srcdir/model1 $rundir/.
ln -sf $srcdir/model2 $rundir/.

cp -f $datadir/namcouple_${src_grid}_${tgt_grid} $rundir/namcouple

cat <<EOF >> $rundir/name_grids.dat
\$grid_source_characteristics
cl_grd_src='$src_grid'
cl_remap='$remap'
\$end
\$grid_target_characteristics
cl_grd_tgt='$tgt_grid'
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

mpirun -np ${nproc_model1} ./model1 : -np ${nproc_model2} ./model2
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

